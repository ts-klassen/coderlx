-module(coderlx_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([simple_response_test/1]).

all() ->
    [simple_response_test].

simple_response_test(_Config) ->
    Hnd0 = coderlx:start(coderlx_opts_builder:sandbox(#{})),
    {{ok, _}, Hnd1} = coderlx:initialize(#{clientInfo => #{name => coderlx, version => <<"0.1.0">>}}, Hnd0),
    {{ok, #{thread := #{id := ThreadId}}}, Hnd2} = coderlx_thread:start(#{}, Hnd1),
    TurnStartParams = #{
            threadId => ThreadId,
            input => [#{type => text, text => <<"Say this is a test.">>}]
        },
    {{ok, #{turn := #{error := null, id := TurnId}}}, Hnd3} = coderlx_turn:start(TurnStartParams, Hnd2),
    {Res, Hnd4} = coderlx:consume(fun
        (timeout, _Acc) ->
            error(timeout);
        ({notification, #{method := 'item/completed', params := #{threadId := ThreadId0, turnId := TurnId0, item := #{type := agentMessage, text := Text}}}}, Acc) when ThreadId0 =:= ThreadId, TurnId0 =:= TurnId ->
            {noreply, {continue, <<Acc/binary, Text/binary>>}};
        ({notification, #{method := 'turn/completed', params := #{threadId := ThreadId0, turn := #{id := TurnId0}}}}, Acc) when ThreadId0 =:= ThreadId, TurnId0 =:= TurnId ->
            {noreply, {break, Acc}};
        (_, Acc) ->
            {noreply, {continue, Acc}}
    end, <<>>, Hnd3),
    coderlx:stop(Hnd4),
    assert_expected_result(<<"This is a test">>, Res),
    ok.

assert_expected_result(Expected, Result) ->
    case binary:match(string:lowercase(Result), string:lowercase(Expected)) of
        nomatch ->
             erlang:error(nomatch, [Expected, Result]);
        _ ->
            ok
    end.
