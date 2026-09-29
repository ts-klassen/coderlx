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
      , default_runtime_path/0
      , default_app_server_path/0
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
      , client_request_method/0
      , server_request_method/0
      , server_request_param/0
      , server_request_response/0
      , respond_error_reason/0
      , server_request_handler/0
    ]).

-klsn_rule_alias([
        {coderlx, {struct, #{
            '?MODULE' => {required, {exact, ?MODULE}}
          , stream => {required, term}
          , buffer => {required, binstr}
          , stderr => {required, term}
          , stream_exit => {optional, term}
          , server_request_handler => {optional, term}
          , pending => {required, {list, term}}
          , next_id => {required, {range, {0, '<', integer}}}
          , send_timeout => {required, timeout}
          , request_timeout => {required, timeout}
          , consume_timeout => {required, timeout}
          , raise_consume_permanent_error => {required, boolean}
          , warn_consume_transient_error => {required, boolean}
        }}}
      , {opts, {struct, #{
            bwrap => {required, term}
          , app_server_path => {optional, binstr}
          , send_timeout => {optional, timeout}
          , server_request_handler => {optional, term}
        }}}
      , {message, {any_of, [
            {alias, {coderlx_app_server_rules, server_request}}
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
-type client_request_method() :: initialize
                               | 'thread/start'
                               | 'thread/resume'
                               | 'thread/fork'
                               | 'thread/archive'
                               | 'thread/delete'
                               | 'thread/unsubscribe'
                               | 'thread/name/set'
                               | 'thread/goal/set'
                               | 'thread/goal/get'
                               | 'thread/goal/clear'
                               | 'thread/metadata/update'
                               | 'thread/attachment/add'
                               | 'thread/attachment/list'
                               | 'thread/attachment/remove'
                               | 'thread/section/move'
                               | 'thread/unarchive'
                               | 'thread/compact/start'
                               | 'thread/shellCommand'
                               | 'thread/approveGuardianDeniedAction'
                               | 'thread/revert'
                               | 'thread/list'
                               | 'threadSection/list'
                               | 'threadSection/create'
                               | 'threadSection/update'
                               | 'threadSection/delete'
                               | 'thread/loaded/list'
                               | 'thread/read'
                               | 'thread/turns/list'
                               | 'thread/items/list'
                               | 'thread/inject_items'
                               | 'skills/list'
                               | 'skills/extraRoots/set'
                               | 'hooks/list'
                               | 'marketplace/add'
                               | 'marketplace/remove'
                               | 'marketplace/upgrade'
                               | 'plugin/list'
                               | 'plugin/installed'
                               | 'plugin/reconcile'
                               | 'plugin/read'
                               | 'plugin/skill/read'
                               | 'plugin/share/save'
                               | 'plugin/share/updateTargets'
                               | 'plugin/share/list'
                               | 'plugin/share/checkout'
                               | 'plugin/share/delete'
                               | 'app/read'
                               | 'app/list'
                               | 'app/installed'
                               | 'fs/readFile'
                               | 'fs/writeFile'
                               | 'fs/createDirectory'
                               | 'fs/getMetadata'
                               | 'fs/readDirectory'
                               | 'fs/remove'
                               | 'fs/copy'
                               | 'fs/watch'
                               | 'fs/unwatch'
                               | 'skills/config/write'
                               | 'plugin/install'
                               | 'plugin/uninstall'
                               | 'turn/start'
                               | 'turn/steer'
                               | 'turn/interrupt'
                               | 'review/start'
                               | 'model/list'
                               | 'account/gatewayOAuth/read'
                               | 'account/gatewayOAuth/login'
                               | 'account/gatewayOAuth/cancel'
                               | 'modelProvider/capabilities/read'
                               | 'experimentalFeature/list'
                               | 'permissionProfile/list'
                               | 'experimentalFeature/enablement/set'
                               | 'mcpServer/oauth/login'
                               | 'config/mcpServer/reload'
                               | 'mcpServerStatus/list'
                               | 'mcpServer/resource/read'
                               | 'mcpServer/tool/call'
                               | 'windowsSandbox/setupStart'
                               | 'windowsSandbox/readiness'
                               | 'account/login/start'
                               | 'account/login/cancel'
                               | 'account/logout'
                               | 'account/rateLimits/read'
                               | 'account/rateLimitResetCredit/consume'
                               | 'account/usage/read'
                               | 'account/workspaceMessages/read'
                               | 'account/sendAddCreditsNudgeEmail'
                               | 'feedback/upload'
                               | 'command/exec'
                               | 'command/exec/write'
                               | 'command/exec/terminate'
                               | 'command/exec/resize'
                               | 'config/read'
                               | 'externalAgentConfig/detect'
                               | 'externalAgentConfig/import'
                               | 'externalAgentConfig/import/recordHistory'
                               | 'externalAgentConfig/import/readHistories'
                               | 'config/value/write'
                               | 'config/batchWrite'
                               | 'configRequirements/read'
                               | 'account/read'
                               | fuzzyFileSearch
                               .
-type server_request_method() :: 'item/commandExecution/requestApproval'
                               | 'item/fileChange/requestApproval'
                               | 'item/tool/requestUserInput'
                               | 'mcpServer/elicitation/request'
                               | 'item/permissions/requestApproval'
                               | 'item/tool/call'
                               | 'account/chatgptAuthTokens/refresh'
                               | 'attestation/generate'
                               .
-type server_request_param() :: klsn:rule(coderlx_app_server_rules, command_execution_request_approval_params)
                              | klsn:rule(coderlx_app_server_rules, file_change_request_approval_params)
                              | klsn:rule(coderlx_app_server_rules, tool_request_user_input_params)
                              | klsn:rule(coderlx_app_server_rules, mcp_server_elicitation_request_params)
                              | klsn:rule(coderlx_app_server_rules, permissions_request_approval_params)
                              | klsn:rule(coderlx_app_server_rules, dynamic_tool_call_params)
                              | klsn:rule(coderlx_app_server_rules, chatgpt_auth_tokens_refresh_params)
                              | klsn:rule(coderlx_app_server_rules, attestation_generate_params)
                              .
-type server_request_response() :: klsn:rule(coderlx_app_server_rules, command_execution_request_approval_response)
                                 | klsn:rule(coderlx_app_server_rules, file_change_request_approval_response)
                                 | klsn:rule(coderlx_app_server_rules, tool_request_user_input_response)
                                 | klsn:rule(coderlx_app_server_rules, mcp_server_elicitation_request_response)
                                 | klsn:rule(coderlx_app_server_rules, permissions_request_approval_response)
                                 | klsn:rule(coderlx_app_server_rules, dynamic_tool_call_response)
                                 | klsn:rule(coderlx_app_server_rules, chatgpt_auth_tokens_refresh_response)
                                 | klsn:rule(coderlx_app_server_rules, attestation_generate_response)
                                 .
-type respond_error_reason() :: unsupported | unhandled | consumer_error.
-type request_id() :: integer() | klsn:binstr().
-type response_deadline() :: integer() | infinity.
-type server_reply() :: {ok, server_request_response()}
                      | {error, respond_error_reason(), jsone:json_value()}.
-type server_request_handler() :: fun(
    ({request, server_request_method(), server_request_param()}) ->
        {ok, server_request_response()} | error | {error, jsone:json_value()} | unhandled
).

-spec start(opts()) -> coderlx().
start(#{bwrap := Bwrap} = Opts) ->
    MaybeHandler = klsn_map:lookup([server_request_handler], Opts),
    case MaybeHandler of
        none -> ok;
        {value, Handler} when is_function(Handler, 1) -> ok;
        {value, _} -> erlang:error(badarg, [Opts])
    end,
    {ok, _} = application:ensure_all_started(coderlx),
    AppServer = maps:get(app_server_path, Opts, default_app_server_path()),
    Stream = klsn_bwrap:open([AppServer], #{
        bwrap => Bwrap
    }),
    Coderlx = #{
        '?MODULE' => ?MODULE
      , stream => Stream
      , buffer => <<>>
      , stderr => <<>>
      , pending => []
      , next_id => 1
      , send_timeout => maps:get(send_timeout, Opts, 60000)
      , request_timeout => 60000
      , consume_timeout => 60000
      , raise_consume_permanent_error => true
      , warn_consume_transient_error => false
    },
    maps:merge(Coderlx, klsn_map:filter(#{server_request_handler => MaybeHandler})).

-spec stop(coderlx()) -> ok.
stop(#{stream := Stream}) ->
    try klsn_bwrap:stop(Stream)
    catch
        %% send/3 already cancels the stream on timeout. Cleanup must not
        %% replace the original exception when the transport has exited.
        error:{badmatch, {error, no_process}} -> ok
    end.

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
    RaisePermanent = maps:get(raise_consume_permanent_error, Coderlx0),
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
        {value, Notification = #{method := error, params := #{ willRetry := false }}} when RaisePermanent ->
            error({consume_permanent_error, Notification});
        {value, Notification} ->
            case {maps:get(warn_consume_transient_error, Coderlx), Notification} of
                {true, #{method := error, params := #{ willRetry := true}}} ->
                    logger:warning(#{
                        mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}
                      , label => consume_transient_error
                      , notification => Notification
                    });
                _ ->
                    ok
            end,
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
        request_id(), respond_error_reason(), jsone:json_value(), coderlx()
    ) -> ok.
respond_error(Id, ReasonEnum, ReasonTerm, #{stream := Stream, send_timeout := Timeout}) ->
    JSON = encode_response(Id, {error, ReasonEnum, ReasonTerm}),
    klsn_bwrap:send(Stream, <<JSON/binary, "\n">>, Timeout),
    ok.


-spec respond_ok(request_id(), server_request_response(), coderlx()) -> ok.
respond_ok(Id, Result, #{stream := Stream, send_timeout := Timeout}) ->
    JSON = encode_response(Id, {ok, Result}),
    klsn_bwrap:send(Stream, <<JSON/binary, "\n">>, Timeout),
    ok.


-spec encode_response(request_id(), server_reply()) -> binary().
encode_response(Id, {ok, Result}) ->
    jsone:encode(#{id => Id, result => Result});
encode_response(Id, {error, ReasonEnum, ReasonTerm}) ->
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
    jsone:encode(Response).

-spec request(client_request_method() | klsn:binstr(), jsone:json_value(), coderlx()) ->
    {jsonrpc_error_or(jsone:json_value()), coderlx()}.
request(Method, Params, Coderlx0) ->
    Id = maps:get(next_id, Coderlx0),
    Stream = maps:get(stream, Coderlx0),
    Message = #{
        id => Id
      , method => Method
      , params => Params
    },
    Normalized = klsn_rule:normalize(
        Message
      , {alias, {coderlx_app_server_rules, client_request}}
    ),
    JSON = klsn_binstr:from_any(jsone:encode(Normalized)),
    klsn_bwrap:send(Stream, <<JSON/binary, "\n">>, maps:get(send_timeout, Coderlx0)),
    %% A timed-out request can still complete later. Reserve its ID before
    %% waiting so the recoverable handle cannot reuse it for a new request.
    Deadline = case maps:get(request_timeout, Coderlx0) of
        infinity -> infinity;
        Timeout -> erlang:monotonic_time(millisecond) + Timeout
    end,
    pop_response(Id, Deadline, Coderlx0#{next_id := Id + 1}).

-spec pop_response(pos_integer(), response_deadline(), coderlx()) ->
    {jsonrpc_error_or(jsone:json_value()), coderlx()}.
pop_response(Id, Deadline, Coderlx0) ->
    MaybeHandler = klsn_map:lookup([server_request_handler], Coderlx0),
    case
        pop_message(
            fun
                (#{id := Id0, result := _} = Message)
                    when Id0 =:= Id, not is_map_key(method, Message) ->
                    true;
                (#{id := Id0, error := _} = Message)
                    when Id0 =:= Id, not is_map_key(method, Message) ->
                    true;
                (#{id := _, method := _}) when MaybeHandler =/= none ->
                    response_time_remaining(Deadline) =/= 0;
                (_) ->
                    false
            end
          , response_time_remaining(Deadline)
          , Coderlx0
        )
    of
        {{value, Request = #{id := ServerId, method := Method}}, Coderlx} ->
            {value, Handler} = MaybeHandler,
            case server_request_rule_(Request) of
                none ->
                    respond_error(ServerId, unsupported, #{}, Coderlx);
                {value, {ParamAlias, ResponseAlias}} ->
                    JSON = try
                        Params = klsn_rule:normalize(
                            maps:get(params, Request)
                          , {alias, {coderlx_app_server_rules, ParamAlias}}
                        ),
                        Reply = case Handler({request, Method, Params}) of
                            {ok, Response} ->
                                {ok, klsn_rule:normalize(
                                    Response
                                  , {alias, {coderlx_app_server_rules, ResponseAlias}}
                                )};
                            error -> {error, consumer_error, #{}};
                            {error, Reason} -> {error, consumer_error, Reason};
                            unhandled -> {error, unhandled, #{}};
                            Other -> erlang:error({invalid_server_request_handler_reply, Other})
                        end,
                        %% JSON metadata may pass schema validation but still
                        %% fail encoding; protect both stages before sending.
                        encode_response(ServerId, Reply)
                    catch
                        Class:Failure:Stack ->
                            respond_error(
                                ServerId
                              , consumer_error
                              , #{message => <<"server_request_handler failed">>}
                              , Coderlx
                            ),
                            erlang:raise(
                                error
                              , {server_request_handler_failed, ServerId, Class, Failure, Coderlx}
                              , Stack
                            )
                    end,
                    %% A transport failure is not a callback failure.
                    klsn_bwrap:send(
                        maps:get(stream, Coderlx)
                      , <<JSON/binary, "\n">>
                      , maps:get(send_timeout, Coderlx)
                    )
            end,
            case response_time_remaining(Deadline) of
                0 -> erlang:error({response_timeout, Id, Coderlx});
                _ -> pop_response(Id, Deadline, Coderlx)
            end;
        {{value, Resp}, Coderlx} ->
            %% Return result or jsonrpc error
            Either = case Resp of
                #{result := Result} ->
                    {ok, Result};
                _ ->
                    {error, Resp}
            end,
            {Either, Coderlx};
        {none, Coderlx} ->
            erlang:error({response_timeout, Id, Coderlx})
    end.

-spec response_time_remaining(response_deadline()) -> timeout().
response_time_remaining(infinity) -> infinity;
response_time_remaining(Deadline) ->
    max(0, Deadline - erlang:monotonic_time(millisecond)).


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
            case klsn_map:lookup([stream_exit], Coderlx20) of
                {value, Reason} -> erlang:error({codex_exit, Reason});
                none -> ok
            end,
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

-spec pop_matching_message_(message_filter(), [message()], [message()]) ->
    klsn:optnl({message(), [message()]}).
pop_matching_message_(_, [], _) ->
    none;
pop_matching_message_(IsTarget, [H|T], Acc) ->
    case IsTarget(H) of
        true ->
            {value, {H, lists:reverse(Acc) ++ T}};
        false ->
            pop_matching_message_(IsTarget, T, [H|Acc])
    end.


-spec dump_stream(coderlx()) -> coderlx().
dump_stream(Coderlx0) ->
    case receive_stream(0, Coderlx0) of
        {value, Coderlx} ->
            dump_stream(Coderlx);
        none ->
            Coderlx0
    end.

-spec receive_stream(timeout(), coderlx()) -> klsn:optnl(coderlx()).
receive_stream(_Timeout, #{stream_exit := _}) ->
    none;
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
        %% Stream completion uses the OS PID, not a monitor reference.
        {'DOWN', OsPid, process, _Pid, Reason} ->
            %% Deliver complete frames already received before reporting EOF.
            {value, Coderlx#{stream_exit => Reason}}
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
            klsn_rule:normalize(
                Json,
                {alias, {coderlx_app_server_rules, server_notification}}
            );
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
      | {mcp_server_elicitation_request_params
        ,mcp_server_elicitation_request_response}
      | {permissions_request_approval_params
        ,permissions_request_approval_response}
      | {dynamic_tool_call_params
        ,dynamic_tool_call_response}
      | {chatgpt_auth_tokens_refresh_params
        ,chatgpt_auth_tokens_refresh_response}
      | {attestation_generate_params
        ,attestation_generate_response}
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
server_request_rule_(#{method := 'mcpServer/elicitation/request'}) ->
    {value, {
        %% McpServerElicitationRequestParams.json
        mcp_server_elicitation_request_params
        %% McpServerElicitationRequestResponse.json
      , mcp_server_elicitation_request_response
    }};
server_request_rule_(#{method := 'item/permissions/requestApproval'}) ->
    {value, {
        %% PermissionsRequestApprovalParams.json
        permissions_request_approval_params
        %% PermissionsRequestApprovalResponse.json
      , permissions_request_approval_response
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
server_request_rule_(#{method := 'attestation/generate'}) ->
    {value, {
        %% AttestationGenerateParams.json
        attestation_generate_params
        %% AttestationGenerateResponse.json
      , attestation_generate_response
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

-spec default_runtime_path() -> klsn:binstr().
default_runtime_path() ->
    klsn_binstr:from_any(filename:join([
        code:priv_dir(coderlx)
      , "codex-runtime"
    ])).

-spec default_app_server_path() -> klsn:binstr().
default_app_server_path() ->
    klsn_binstr:from_any(filename:join([
        default_runtime_path()
      , "bin"
      , "codex-app-server"
    ])).


test() ->
    C0 = start(coderlx_opts_builder:sandbox(#{})),
    {R10, C10} = initialize(#{clientInfo => #{name => coderlx, version => <<"0.1.0">>}}, C0),
    {R20, C20} = coderlx_thread:start(#{}, C10),
    {ok, #{thread := #{id := ThreadId}}} = R20,
    TurnParams = #{
        threadId => ThreadId,
      % input => [#{type => text, text => <<"Say this is a test">>}]
        input => [#{type => text, text => <<"Create hello.txt with hello world. Tell me the exact path you created the file.">>}]
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
