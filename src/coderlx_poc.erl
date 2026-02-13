-module(coderlx_poc).
-include_lib("klsn/include/klsn_rule_annotation.hrl").

-export([
    start/1,
    stop/1,
    send_request/3,
    send_notification/3,
    await_response/3,
    recv/2,
    initialize/3,
    initialized/1,
    default_codex_path/0
]).

-export_type([
    conn_opts/0,
    conn/0,
    request_id/0,
    client_info/0,
    server_message/0,
    maybe_message/0,
    maybe_response/0
]).

-type bwrap_opts() :: [klsn_bwrap:bwrap_opt()].

-type conn_opts() :: #{
    bwrap := bwrap_opts(),
    codex_path => klsn:binstr(),
    codex_args => [klsn:binstr()]
}.

-type conn() :: #{
    stream := klsn_bwrap:stream(),
    buffer := binary(),
    pending := [server_message()],
    next_id := integer(),
    opts := conn_opts()
}.

-type request_id() :: integer() | klsn:binstr().

-type client_info() :: #{
    name := klsn:binstr(),
    title => klsn:binstr(),
    version := klsn:binstr()
}.

-type server_request() :: map().
-type server_notification() :: map().
-type codex_event_notification() :: map().
-type jsonrpc_request() :: map().
-type jsonrpc_notification() :: map().
-type jsonrpc_response() :: map().
-type jsonrpc_error() :: map().
-type server_message() ::
    server_request() |
    server_notification() |
    codex_event_notification() |
    jsonrpc_request() |
    jsonrpc_notification() |
    jsonrpc_response() |
    jsonrpc_error().

-type maybe_message() :: klsn:optnl(server_message()).
-type maybe_response() :: klsn:optnl(jsonrpc_response() | jsonrpc_error()).

-klsn_rule_alias([
    {bwrap_opts, {list, term}},
    {conn_opts, {struct, #{
        bwrap => {required, {alias, {?MODULE, bwrap_opts}}},
        codex_path => {optional, binstr},
        codex_args => {optional, {list, binstr}}
    }}},
    {conn, term},
    {request_id, {any_of, [integer, binstr]}},
    {client_info, {struct, #{
        name => {required, binstr},
        title => {optional, binstr},
        version => {required, binstr}
    }}},
    {server_request, {alias, {coderlx_app_server_rules, server_request_from_json}}},
    {server_notification, {alias, {coderlx_app_server_rules, server_notification_from_json}}},
    {codex_event_notification, {alias, {coderlx_app_server_rules, codex_event_notification_from_json}}},
    {jsonrpc_request, {alias, {coderlx_app_server_rules, jsonrpc_request_from_json}}},
    {jsonrpc_notification, {alias, {coderlx_app_server_rules, jsonrpc_notification_from_json}}},
    {jsonrpc_response, {alias, {coderlx_app_server_rules, jsonrpc_response_from_json}}},
    {jsonrpc_error, {alias, {coderlx_app_server_rules, jsonrpc_error_from_json}}},
    {server_message, {any_of, [
        {alias, {?MODULE, server_request}},
        {alias, {?MODULE, server_notification}},
        {alias, {?MODULE, codex_event_notification}},
        {alias, {?MODULE, jsonrpc_request}},
        {alias, {?MODULE, jsonrpc_notification}},
        {alias, {?MODULE, jsonrpc_response}},
        {alias, {?MODULE, jsonrpc_error}}
    ]}},
    {maybe_message, {optnl, {alias, {?MODULE, server_message}}}},
    {maybe_response, {optnl, {any_of, [
        {alias, {?MODULE, jsonrpc_response}},
        {alias, {?MODULE, jsonrpc_error}}
    ]}}}
]).

-klsn_input_rule([{alias, {?MODULE, conn_opts}}]).
-klsn_output_rule({alias, {?MODULE, conn}}).
-spec start(conn_opts()) -> conn().
start(Opts) ->
    {ok, _} = application:ensure_all_started(coderlx),
    BwrapOpts = maps:get(bwrap, Opts),
    CodexPath = case klsn_map:lookup([codex_path], Opts) of
        {value, CodexPath0} ->
            klsn_binstr:from_any(CodexPath0);
        none ->
            default_codex_path()
    end,
    CodexArgs = case klsn_map:lookup([codex_args], Opts) of
        none ->
            [];
        {value, Args0} when is_list(Args0) ->
            [klsn_binstr:from_any(Arg) || Arg <- Args0];
        {value, _} ->
            erlang:error({badarg, Opts})
    end,
    Stream = klsn_bwrap:open(build_command(CodexPath, CodexArgs), #{
        bwrap => BwrapOpts
    }),
    #{
        stream => Stream,
        buffer => <<>>,
        pending => [],
        next_id => 1,
        opts => Opts#{codex_path => CodexPath, codex_args => CodexArgs}
    }.

-klsn_input_rule([{alias, {?MODULE, conn}}]).
-klsn_output_rule({exact, ok}).
-spec stop(conn()) -> ok.
stop(#{stream := Stream}) ->
    klsn_bwrap:stop(Stream).

-klsn_input_rule([{alias, {?MODULE, conn}}, atom, term]).
-klsn_output_rule({tuple, [{alias, {?MODULE, request_id}}, {alias, {?MODULE, conn}}]}).
-spec send_request(conn(), atom(), term()) -> {request_id(), conn()}.
send_request(Conn0, Method, Params) when is_atom(Method) ->
    {RequestId, Conn1} = next_request_id(Conn0),
    Request = #{
        id => RequestId,
        method => Method,
        params => Params
    },
    ok = send_json(Conn1, Request, client_request_to_json),
    {RequestId, Conn1};
send_request(_Conn0, Method, _Params) ->
    erlang:error({badarg, Method}).

-klsn_input_rule([{alias, {?MODULE, conn}}, atom, {optnl, term}]).
-klsn_output_rule({alias, {?MODULE, conn}}).
-spec send_notification(conn(), atom(), klsn:optnl(term())) -> conn().
send_notification(Conn, Method, ParamsOpt) when is_atom(Method) ->
    Notification = case ParamsOpt of
        none ->
            #{method => Method};
        {value, Params} ->
            #{method => Method, params => Params}
    end,
    ok = send_json(Conn, Notification, client_notification_to_json),
    Conn;
send_notification(_Conn, Method, _ParamsOpt) ->
    erlang:error({badarg, Method}).

-klsn_input_rule([{alias, {?MODULE, conn}}, {alias, {?MODULE, request_id}}, timeout]).
-klsn_output_rule({tuple, [{alias, {?MODULE, maybe_response}}, {alias, {?MODULE, conn}}]}).
-spec await_response(conn(), request_id(), timeout()) -> {maybe_response(), conn()}.
await_response(Conn, RequestId, Timeout) ->
    case take_pending_response(RequestId, Conn) of
        {value, Response, Conn1} ->
            {{value, Response}, Conn1};
        none ->
            Deadline = timeout_deadline(Timeout),
            PendingRev = lists:reverse(maps:get(pending, Conn)),
            Conn1 = Conn#{pending := []},
            await_response_loop(Conn1, RequestId, Deadline, PendingRev)
    end.

-klsn_input_rule([{alias, {?MODULE, conn}}, timeout]).
-klsn_output_rule({tuple, [{alias, {?MODULE, maybe_message}}, {alias, {?MODULE, conn}}]}).
-spec recv(conn(), timeout()) -> {maybe_message(), conn()}.
recv(#{pending := [Message | Rest]} = Conn, _Timeout) ->
    {{value, Message}, Conn#{pending := Rest}};
recv(Conn, Timeout) ->
    recv_loop(Conn, Timeout).

-klsn_input_rule([{alias, {?MODULE, conn}}, {alias, {?MODULE, client_info}}, timeout]).
-klsn_output_rule({tuple, [{alias, {?MODULE, maybe_response}}, {alias, {?MODULE, conn}}]}).
-spec initialize(conn(), client_info(), timeout()) -> {maybe_response(), conn()}.
initialize(Conn0, ClientInfo, Timeout) ->
    Params = #{clientInfo => ClientInfo},
    {RequestId, Conn1} = send_request(Conn0, initialize, Params),
    await_response(Conn1, RequestId, Timeout).

-klsn_input_rule([{alias, {?MODULE, conn}}]).
-klsn_output_rule({alias, {?MODULE, conn}}).
-spec initialized(conn()) -> conn().
initialized(Conn) ->
    send_notification(Conn, initialized, none).

-klsn_input_rule([]).
-klsn_output_rule(binstr).
-spec default_codex_path() -> klsn:binstr().
default_codex_path() ->
    PrivDir = case code:priv_dir(coderlx) of
        {error, bad_name} ->
            erlang:error({priv_dir_not_found, coderlx});
        Dir ->
            Dir
    end,
    klsn_binstr:from_any(filename:join([PrivDir, "codex"])).

build_command(CodexPath, CodexArgs) ->
    [CodexPath, <<"app-server">> | CodexArgs].

next_request_id(#{next_id := NextId} = Conn) ->
    {NextId, Conn#{next_id := NextId + 1}}.

send_json(#{stream := Stream}, Message, RuleAlias) ->
    Normalized = klsn_rule:normalize(Message, {alias, {coderlx_app_server_rules, RuleAlias}}),
    Json = jsone:encode(Normalized),
    Line = <<(klsn_binstr:from_any(Json))/binary, $\n>>,
    klsn_bwrap:send(Stream, Line).

recv_loop(#{stream := #{os_pid := OsPid}} = Conn, Timeout) ->
    %% Timeout is per-receive idle time, not a total deadline.
    receive
        {stdout, OsPid, Data} when is_binary(Data) ->
            Buffer = maps:get(buffer, Conn),
            {Messages, Rest} = decode_stdout(<<Buffer/binary, Data/binary>>),
            Conn1 = Conn#{buffer := Rest},
            case Messages of
                [] ->
                    recv_loop(Conn1, Timeout);
                [Message | Tail] ->
                    Pending = maps:get(pending, Conn1) ++ Tail,
                    {{value, Message}, Conn1#{pending := Pending}}
            end;
        {stderr, OsPid, _Data} ->
            recv_loop(Conn, Timeout);
        {'DOWN', OsPid, process, _Pid, Reason} ->
            erlang:error({codex_exit, Reason})
    after Timeout ->
        {none, Conn}
    end.

decode_stdout(Data) ->
    Parts = binary:split(Data, <<"\n">>, [global]),
    {Lines, Rest} = case lists:reverse(Parts) of
        [] ->
            {[], <<>>};
        [Last | RevLines] ->
            {lists:reverse(RevLines), Last}
    end,
    Messages = lists:reverse(lists:foldl(fun(Line, Acc) ->
        case Line of
            <<>> ->
                Acc;
            _ ->
                [decode_message(Line) | Acc]
        end
    end, [], Lines)),
    {Messages, Rest}.

decode_message(Line) ->
    Json = jsone:decode(Line),
    case classify_message(Json) of
        request ->
            klsn_rule:normalize(Json, {alias, {coderlx_app_server_rules, server_request_from_json}});
        notification ->
            case maps:get(<<"method">>, Json) of
                <<"codex/event/", _/binary>> ->
                    klsn_rule:normalize(
                        Json,
                        {alias, {coderlx_app_server_rules, codex_event_notification_from_json}}
                    );
                _ ->
                    klsn_rule:normalize(
                        Json,
                        {alias, {coderlx_app_server_rules, server_notification_from_json}}
                    )
            end;
        response ->
            klsn_rule:normalize(Json, {alias, {coderlx_app_server_rules, jsonrpc_response_from_json}});
        error ->
            klsn_rule:normalize(Json, {alias, {coderlx_app_server_rules, jsonrpc_error_from_json}});
        unknown ->
            erlang:error({unknown_message, Json})
    end.

classify_message(Json) when is_map(Json) ->
    HasMethod = maps:is_key(<<"method">>, Json),
    HasId = maps:is_key(<<"id">>, Json),
    HasResult = maps:is_key(<<"result">>, Json),
    HasError = maps:is_key(<<"error">>, Json),
    case {HasMethod, HasId, HasResult, HasError} of
        {true, true, _, _} -> request;
        {true, false, _, _} -> notification;
        {false, true, true, _} -> response;
        {false, true, _, true} -> error;
        _ -> unknown
    end;
classify_message(_) ->
    unknown.

take_pending_response(RequestId, #{pending := Pending} = Conn) ->
    take_pending_response_(RequestId, Pending, [], Conn).

take_pending_response_(_RequestId, [], _AccRev, _Conn) ->
    none;
take_pending_response_(RequestId, [Message | Rest], AccRev, Conn) ->
    case response_matches_id(Message, RequestId) of
        true ->
            Pending = lists:reverse(AccRev) ++ Rest,
            {value, Message, Conn#{pending := Pending}};
        false ->
            take_pending_response_(RequestId, Rest, [Message | AccRev], Conn)
    end.

response_matches_id(Message, RequestId) ->
    case klsn_map:lookup([result], Message) of
        {value, _} ->
            message_id_equals(Message, RequestId);
        none ->
            case klsn_map:lookup([error], Message) of
                {value, _} -> message_id_equals(Message, RequestId);
                none -> false
            end
    end.

message_id_equals(Message, RequestId) ->
    case klsn_map:lookup([id], Message) of
        {value, Id} -> Id =:= RequestId;
        none -> false
    end.

await_response_loop(Conn0, RequestId, infinity, PendingRev) ->
    case recv(Conn0, infinity) of
        {none, Conn1} ->
            {none, Conn1#{pending := lists:reverse(PendingRev)}};
        {{value, Message}, Conn1} ->
            case response_matches_id(Message, RequestId) of
                true ->
                    {{value, Message}, Conn1#{pending := lists:reverse(PendingRev)}};
                false ->
                    await_response_loop(Conn1, RequestId, infinity, [Message | PendingRev])
            end
    end;
await_response_loop(Conn0, RequestId, Deadline, PendingRev) ->
    Remaining = timeout_remaining(Deadline),
    case Remaining =< 0 of
        true ->
            {none, Conn0#{pending := lists:reverse(PendingRev)}};
        false ->
            case recv(Conn0, Remaining) of
                {none, Conn1} ->
                    {none, Conn1#{pending := lists:reverse(PendingRev)}};
                {{value, Message}, Conn1} ->
                    case response_matches_id(Message, RequestId) of
                        true ->
                            {{value, Message}, Conn1#{pending := lists:reverse(PendingRev)}};
                        false ->
                            await_response_loop(Conn1, RequestId, Deadline, [Message | PendingRev])
                    end
            end
    end.

timeout_deadline(infinity) ->
    infinity;
timeout_deadline(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 ->
    erlang:monotonic_time(millisecond) + TimeoutMs;
timeout_deadline(Timeout) ->
    erlang:error({invalid_timeout, Timeout}).

timeout_remaining(Deadline) when is_integer(Deadline) ->
    Deadline - erlang:monotonic_time(millisecond).
