-module(coderlx).
-include_lib("klsn/include/klsn_rule_annotation.hrl").
-include("coderlx_parameterized_rules.hrl").

%% Public functions
-export([
        start/1
      , stop/1
      , initialize/2
      , consume/3
      , consume/4
      , default_codex_path/0
    ]).

%% Low level api
-export([
        request/3
      , pop_message/3
      , respond_ok/3
      , respond_error/4
    ]).

%% Temporary test functions for dev
-export([
        test/0
    ]).

-export_type([
        coderlx/0
      , opts/0
      , message/0
      , jsonrpc_error_or/1
      , message_filter/0
      , consumer_entry/0
      , consumer/1
      , server_request_method/0
      , server_request_param/0
      , server_request_response/0
      , respond_error_reason/0
    ]).

-klsn_rule_alias([
        {coderlx, {struct, #{
            '?MODULE' => {required, {exact, ?MODULE}}
          , stream => {required, term}
          , buffer => {required, binstr}
          , stderr => {required, term}
          , pending => {required, {list, term}}
          , next_id => {required, {range, {0, '<', integer}}}
          , request_timeout => {required, timeout}
          , consume_timeout => {required, timeout}
        }}}
      , {opts, {struct, #{
            bwrap => {required, term}
          , codex_path => {optional, binstr}
        }}}
      , {message, {any_of, [
            {alias, {coderlx_app_server_rules, server_request}}
          , {alias, {coderlx_app_server_rules, codex_event_notification}}
          , {alias, {coderlx_app_server_rules, server_notification}}
          , {alias, {coderlx_app_server_rules, jsonrpc_response}}
          , {alias, {coderlx_app_server_rules, jsonrpc_error}}
        ]}}
    ]).

-opaque coderlx() :: klsn_rule:alias(coderlx).
-type opts() :: klsn_rule:alias(opts).
-type message() :: klsn_rule:alias(message).
-type jsonrpc_error_or(Response) :: {ok, Response} | {error, klsn_rule:alias(coderlx_app_server_rules, jsonrpc_error)}.
-type message_filter() :: fun((message()) -> boolean()).
-type consumer_entry() :: {request, server_request_method(), server_request_param()}
                        | {notification, message()}
                        | timeout
                        .
-type consumer(Acc) :: fun(
                           (consumer_entry(), Acc) ->
                               %% only valid for request
                               {reply, {ok, server_request_response()} | error | {error, term()}, {continue|break, Acc}}
                               %% notification and timeout return
                             | {noreply, {continue|break, Acc}}
                       ).
-type server_request_method() :: 'item/commandExecution/requestApproval'
                               | 'item/fileChange/requestApproval'
                               | 'item/tool/requestUserInput'
                               | 'item/tool/call'
                               | 'account/chatgptAuthTokens/refresh'
                               .
-type server_request_param() :: klsn:rule(coderlx_app_server_rules, command_execution_request_approval_params)
                              | klsn:rule(coderlx_app_server_rules, file_change_request_approval_params)
                              | klsn:rule(coderlx_app_server_rules, tool_request_user_input_params)
                              | klsn:rule(coderlx_app_server_rules, dynamic_tool_call_params)
                              | klsn:rule(coderlx_app_server_rules, chatgpt_auth_tokens_refresh_params)
                              .
-type server_request_response() :: klsn:rule(coderlx_app_server_rules, command_execution_request_approval_response)
                                 | klsn:rule(coderlx_app_server_rules, file_change_request_approval_response)
                                 | klsn:rule(coderlx_app_server_rules, tool_request_user_input_response)
                                 | klsn:rule(coderlx_app_server_rules, dynamic_tool_call_response)
                                 | klsn:rule(coderlx_app_server_rules, chatgpt_auth_tokens_refresh_response)
                                 .
-type respond_error_reason() :: unsupported | unhandled | consumer_error.

-spec start(opts()) -> coderlx().
start(#{bwrap := Bwrap}=Opts) ->
    {ok, _} = application:ensure_all_started(coderlx),
    Codex = maps:get(codex_path, Opts, default_codex_path()),
    Stream = klsn_bwrap:open([Codex, <<"app-server">>], #{
        bwrap => Bwrap
    }),
    #{
        '?MODULE' => ?MODULE
      , stream => Stream
      , buffer => <<>>
      , stderr => <<>>
      , pending => []
      , next_id => 1
      , request_timeout => 60000
      , consume_timeout => 60000
    }.

-spec stop(coderlx()) -> ok.
stop(#{stream := Stream}) ->
    klsn_bwrap:stop(Stream).

-klsn_input_rule([
        {alias, {coderlx_app_server_rules, initialize_params}}
      , {alias, {?MODULE, coderlx}}
    ]).
-klsn_output_rule({tuple, [
        ?JSONRPC_ERROR_OR(initialize_response)
      , {alias, {?MODULE, coderlx}}
    ]}).
initialize(Params, Coderlx) ->
    request(initialize, Params, Coderlx).

-spec consume(consumer(Acc), Acc, coderlx()) -> {Acc, coderlx()}.
consume(Consumer, Acc, Coderlx) ->
    consume(Consumer, Acc, fun(_)-> true end, Coderlx).

-spec consume(
        consumer(Acc), Acc, message_filter(), coderlx()
    ) -> {Acc, coderlx()}.
consume(Consumer, Acc0, Filter, Coderlx0) ->
    Timeout = maps:get(consume_timeout, Coderlx0),
    {MaybeMessage, Coderlx} = pop_message(Filter, Timeout, Coderlx0),
    Consumed = case MaybeMessage of
        none ->
            {noreply, Consumed0} = Consumer(timeout, Acc0),
            Consumed0;
        {value, Request = #{id := Id, method := Method}} ->
            case server_request_rule_(Request) of
                none ->
                    respond_error(Id, unsupported, #{}, Coderlx),
                    {continue, Acc0};
                {value, {ParamRuleAlias, ResponseRuleAlias}} ->
                    ParamRule = {alias, {coderlx_app_server_rules, ParamRuleAlias}},
                    ResponseRule = {alias, {coderlx_app_server_rules, ResponseRuleAlias}},
                    Param = klsn_rule:normalize(maps:get(params, Request), ParamRule),
                    case Consumer({request, Method, Param}, Acc0) of
                        {noreply, Consumed0} ->
                            respond_error(Id, unhandled, #{}, Coderlx),
                            Consumed0;
                        {reply, error, Consumed0} ->
                            respond_error(Id, consumer_error, #{}, Coderlx),
                            Consumed0;
                        {reply, {error, Reason}, Consumed0} ->
                            respond_error(Id, consumer_error, Reason, Coderlx),
                            Consumed0;
                        {reply, {ok, Response0}, Consumed0} ->
                            Response = klsn_rule:normalize(Response0, ResponseRule),
                            respond_ok(Id, Response, Coderlx),
                            Consumed0
                    end
            end;
        {value, Notification} ->
            {noreply, Consumed0} = Consumer({notification, Notification}, Acc0),
            Consumed0
    end,
    case Consumed of
        {continue, Acc} ->
            consume(Consumer, Acc, Filter, Coderlx);
        {break, Acc} ->
            {Acc, Coderlx}
    end.


-spec respond_error(
        klsn:binstr(), respond_error_reason(), term(), coderlx()
    ) -> ok.
respond_error(Id, ReasonEnum, ReasonTerm, #{stream:=Stream}) ->
    {Code, Msg} = case ReasonEnum of
        unsupported ->
            {-32601, <<"unsupported: Method is not supported by github.com/ts-klassen/coderlx.">>};
        consumer_error ->
            {-32000, <<"consumer_error:  Method is supported by github.com/ts-klassen/coderlx, but the consumer returned an error.">>};
        unhandled ->
            {-32001, <<"unhandled: Method is supported by github.com/ts-klassen/coderlx, but the consumer did not handle the request.">>};
        _ ->
            {-32603, <<"_: coderlx:respond_error/3 called with invalid reason.">>}
    end,
    Response = #{
        id => Id
      , error => #{
            code => Code
          , message => Msg
          , data => ReasonTerm
        }
    },
    JSON = klsn_binstr:from_any(jsone:encode(Response)),
    klsn_bwrap:send(Stream, <<JSON/binary, "\n">>),
    ok.


-spec respond_ok(klsn:binstr(), server_request_response(), coderlx()) -> ok.
respond_ok(Id, Result, #{stream:=Stream}) ->
    Response = #{
        id => Id
      , result => Result
    },
    JSON = klsn_binstr:from_any(jsone:encode(Response)),
    klsn_bwrap:send(Stream, <<JSON/binary, "\n">>),
    ok.



request(Method, Params, Coderlx0) ->
    Id = maps:get(next_id, Coderlx0),
    Stream = maps:get(stream, Coderlx0),
    Message = #{
        id => Id,
        method => Method,
        params => Params
    },
    Normalized = klsn_rule:normalize(
        Message
      , {alias, {coderlx_app_server_rules, client_request}}
    ),
    JSON = klsn_binstr:from_any(jsone:encode(Normalized)),
    klsn_bwrap:send(Stream, <<JSON/binary, "\n">>),
    {Response, Coderlx} = pop_response(Id, Coderlx0),
    {Response, Coderlx#{
        next_id := Id + 1
    }}.

-spec pop_response(
        pos_integer(), coderlx()
    ) -> {jsonrpc_error_or(term()), coderlx()}.
pop_response(Id, Coderlx0) ->
    case
        pop_message(
            fun
                (#{id := Id0}) when Id0 =:= Id ->
                    true;
                (_) ->
                    false
            end
          , maps:get(request_timeout, Coderlx0)
          , Coderlx0
        )
    of
        {{value, Resp}, Coderlx} ->
            %% Return result or jsonrpc error
            Either = case Resp of
                #{result := Result} ->
                    {ok, Result};
                _ ->
                    {error, Resp}
            end,
            {Either, Coderlx};
        {none, _} ->
            erlang:error(response_timeout, [Id, Coderlx0])
    end.


-spec pop_message(
        message_filter()
      , timeout()
      , coderlx()
    ) -> {klsn:optnl(message()), coderlx()}.
pop_message(IsTarget, Timeout, Coderlx0) ->
    StartTime = erlang:monotonic_time(millisecond),
    Coderlx10 = dump_stream(Coderlx0),
    Coderlx20 = decode_buffer(Coderlx10),
    Pending0 = maps:get(pending, Coderlx20),
    case pop_matching_message_(IsTarget, Pending0, []) of
        {value, {Message, Pending}} ->
            {{value, Message}, Coderlx20#{ pending := Pending }};
        none ->
            case receive_stream(Timeout, Coderlx20) of
                {value, Coderlx30} ->
                    NextTimeout = case Timeout of
                        infinity -> infinity;
                        _ ->
                            max(0, Timeout - erlang:monotonic_time(millisecond) + StartTime)
                    end,
                    pop_message(IsTarget, NextTimeout, Coderlx30);
                none ->
                    {none, Coderlx20}
            end
    end.

pop_matching_message_(_, [], _) ->
    none;
pop_matching_message_(IsTarget, [H|T], Acc) ->
    case IsTarget(H) of
        true ->
            {value, {H, lists:reverse(Acc) ++ T}};
        false ->
            pop_matching_message_(IsTarget, T, [H|Acc])
    end.


dump_stream(Coderlx0) ->
    case receive_stream(0, Coderlx0) of
        {value, Coderlx} ->
            dump_stream(Coderlx);
        none ->
            Coderlx0
    end.

receive_stream(Timeout, Coderlx = #{stream := #{os_pid := OsPid}, buffer := StdOut, stderr := StdErr}) ->
    receive
        {stdout, OsPid, Data} ->
            {value, Coderlx#{
                buffer := <<StdOut/binary, Data/binary>>
              , stderr := StdErr
            }};
        {stderr, OsPid, Data} ->
            {value, Coderlx#{
                buffer := StdOut
              , stderr := <<StdErr/binary, Data/binary>>
            }};
        %% DOWN from erlexec. (Not monitor.)
        {'DOWN', OsPid, process, _Pid, Reason} ->
            erlang:error({codex_exit, Reason})
    after Timeout ->
        none
    end.

decode_buffer(Coderlx = #{ buffer := Data, pending := Pending}) ->
    Parts = binary:split(Data, <<"\n">>, [global]),
    {RevLines, Rest} = case lists:reverse(Parts) of
        [] ->
            {[], <<>>};
        [Last | RevLines0] ->
            {lists:reverse(RevLines0), Last}
    end,
    Messages = lists:foldr(fun(Line, Acc) ->
        case Line of
            <<>> ->
                Acc;
            _ ->
                [decode_message(Line) | Acc]
        end
    end, [], RevLines),
    Coderlx#{
        buffer := Rest
      , pending := Pending ++ Messages
    }.

decode_message(Line) ->
    Json = jsone:decode(Line),
    case classify_message(Json) of
        request ->
            klsn_rule:normalize(Json, {alias, {coderlx_app_server_rules, server_request}});
        notification ->
            case maps:get(<<"method">>, Json) of
                <<"codex/event/", _/binary>> ->
                    klsn_rule:normalize(
                        Json,
                        {alias, {coderlx_app_server_rules, codex_event_notification}}
                    );
                _ ->
                    klsn_rule:normalize(
                        Json,
                        {alias, {coderlx_app_server_rules, server_notification}}
                    )
            end;
        response ->
            klsn_rule:normalize(Json, {alias, {coderlx_app_server_rules, jsonrpc_response}});
        error ->
            klsn_rule:normalize(Json, {alias, {coderlx_app_server_rules, jsonrpc_error}});
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

-spec server_request_rule_(message()) -> klsn:optnl(
        {command_execution_request_approval_params
        ,command_execution_request_approval_response}
      | {file_change_request_approval_params
        ,file_change_request_approval_response}
      | {tool_request_user_input_params
        ,tool_request_user_input_response}
      | {dynamic_tool_call_params
        ,dynamic_tool_call_response}
      | {chatgpt_auth_tokens_refresh_params
        ,chatgpt_auth_tokens_refresh_response}
    ).
server_request_rule_(#{method := 'item/commandExecution/requestApproval'}) ->
    {value, {
        %% CommandExecutionRequestApprovalParams.json
        command_execution_request_approval_params
        %% CommandExecutionRequestApprovalResponse.json
      , command_execution_request_approval_response
    }};
server_request_rule_(#{method := 'item/fileChange/requestApproval'}) ->
    {value, {
        %% FileChangeRequestApprovalParams.json
        file_change_request_approval_params
        %% FileChangeRequestApprovalResponse.json
      , file_change_request_approval_response
    }};
server_request_rule_(#{method := 'item/tool/requestUserInput'}) ->
    {value, {
        %% ToolRequestUserInputParams.json
        tool_request_user_input_params
        %% ToolRequestUserInputResponse.json
      , tool_request_user_input_response
    }};
server_request_rule_(#{method := 'item/tool/call'}) ->
    {value, {
        %% DynamicToolCallParams.json
        dynamic_tool_call_params
        %% DynamicToolCallResponse.json
      , dynamic_tool_call_response
    }};
server_request_rule_(#{method := 'account/chatgptAuthTokens/refresh'}) ->
    {value, {
        %% ChatgptAuthTokensRefreshParams.json
        chatgpt_auth_tokens_refresh_params
        %% ChatgptAuthTokensRefreshResponse.json
      , chatgpt_auth_tokens_refresh_response
    }};
%% Try to keep up to date with new methods. Add them above this comment.
%% Update these types if you add more.
%% -type server_request_method()
%% -type server_request_param()
%% -type server_request_response()
%% We NEVER support legacy methods like:
%%  - applyPatchApproval
%%  - execCommandApproval
server_request_rule_(_) ->
    none.

-spec default_codex_path() -> klsn:binstr().
default_codex_path() ->
    klsn_binstr:from_any(filename:join([
        code:priv_dir(coderlx)
      , "codex"
    ])).


test() ->
    C0 = start(#{
        bwrap => [
            {tmpfs, <<"/">>}
          , {proc, <<"/proc">>}
          , {dev, <<"/dev">>}
          , {dir, <<"/tmp">>}
          , share_net
          , new_session
          , die_with_parent
          , clearenv
          , {setenv, <<"HOME">>, <<"/home/codex">>}
          , {setenv, <<"USER">>, <<"codex">>}
          , {setenv, <<"LOGNAME">>, <<"codex">>}
          , {setenv, <<"PATH">>, <<"/cbin:/usr/bin:/bin">>}
          , {setenv, <<"SHELL">>, <<"/bin/bash">>}
          , {setenv, <<"TMPDIR">>, <<"/tmp">>}
          , {dir, <<"/home">>}
          , {dir, <<"/home/codex">>}
          , {dir, <<"/home/codex/work">>}
          , {ro_bind, <<"/usr">>, <<"/usr">>}
          , {ro_bind, <<"/bin">>, <<"/bin">>}
          , {ro_bind, <<"/lib">>, <<"/lib">>}
          , {ro_bind, <<"/lib64">>, <<"/lib64">>}
          , {ro_bind, <<"/etc">>, <<"/etc">>}
          , {ro_bind, <<"/run">>, <<"/run">>}
          , {ro_bind, <<"/sys">>, <<"/sys">>}
          , {ro_bind, default_codex_path(), <<"/cbin/codex">>}
          , {bind, <<"/home/codex/.codex">>, <<"/home/codex/.codex">>}
        ]
      , codex_path => <<"/cbin/codex">>
    }),
    {R10, C10} = initialize(#{clientInfo => #{name => coderlx, version => <<"0.1.0">>}}, C0),
    {R20, C20} = coderlx_thread:start(#{}, C10),
    {ok, #{thread := #{id := ThreadId}}} = R20,
    TurnParams = #{
        threadId => ThreadId,
      % input => [#{type => text, text => <<"Say this is a test">>}]
        input => [#{type => text, text => <<"Create hello.txt with hello world">>}]
    },
    {R30, C30} = coderlx_turn:start(TurnParams, C20),
    {R40, C40} = consume(fun
        (Msg = {request, 'item/commandExecution/requestApproval', _}, Acc0) ->
            io:format("approved:~n    ~p~n", [Msg]),
            Result = #{
                decision => accept
            },
            {reply, {ok, Result}, {continue, Acc0}};
        (Msg = {notification, #{method := 'turn/completed'}}, _) ->
            {noreply, {break, Msg}};
        (Msg, Acc0) ->
            io:format("ignored message:~n    ~p~n", [Msg]),
            {noreply, {continue, Acc0}}
    end, [], C30),
    {none, C} = pop_message(fun(_)->false end, 0, C40),
    stop(C),
    {[R10, R20, R30, R40], C}.
