#!/usr/bin/env escript
%%! +S 2:2
-mode(compile).

-spec main([string()]) -> ok.
main(_) ->
    true = code:add_patha(os:getenv("CODERLX_TEST_JSON_EBIN")),
    io:format(standard_error, "ready~n", []),
    #{<<"id">> := RequestId, <<"params">> := Params} = read(),
    case Params of
        #{<<"clientInfo">> := #{<<"name">> := <<"timeout-data">>}} ->
            send(#{method => 'fs/changed', params => #{
                watchId => <<"watch-1">>
              , changedPaths => [<<"/tmp/changed">>]
            }}),
            io:put_chars("{\"id\":1,\"result\":"),
            io:format(standard_error, "timeout-data~n", []),
            #{<<"id">> := FollowingId} = read(),
            io:put_chars("{}}\n"),
            send(#{id => FollowingId, result => #{}});
        #{<<"tool">> := <<"deadline">>} ->
            lists:foreach(fun(Id) ->
                send(#{id => Id, method => 'attestation/generate', params => #{}}),
                read()
            end, lists:seq(10, 14)),
            send(#{id => RequestId, result => #{content => []}});
        _ ->
            send(#{id => 0, method => 'mcpServer/elicitation/request', params => #{
                threadId => <<"thread-1">>
              , serverName => <<"fixture">>
              , mode => form
              , message => <<"Test question">>
              , requestedSchema => #{
                    type => object
                  , properties => #{answer => #{type => string}}
                }
            }}),
            Reply = read(),
            send(#{id => RequestId, result => #{content => [], structuredContent => Reply}})
    end,
    wait_for_stop().

-spec send(jsone:json_value()) -> ok.
send(Message) ->
    io:put_chars([jsone:encode(Message), "\n"]).

-spec read() -> jsone:json_value().
read() ->
    jsone:decode(unicode:characters_to_binary(io:get_line(""))).

-spec wait_for_stop() -> ok.
wait_for_stop() ->
    case io:get_line("") of
        eof -> ok;
        _ -> wait_for_stop()
    end.
