-module(coderlx_app_server_rules_tests).

-include_lib("eunit/include/eunit.hrl").

mcp_server_elicitation_request_context_test_() ->
    [
        {"form", fun() ->
            assert_mcp_elicitation_context(#{
                <<"mode">> => <<"form">>,
                <<"requestedSchema">> => #{
                    <<"type">> => <<"object">>,
                    <<"properties">> => #{}
                }
            })
        end},
        {"openai/form", fun() ->
            assert_mcp_elicitation_context(#{
                <<"mode">> => <<"openai/form">>,
                <<"requestedSchema">> => #{}
            })
        end},
        {"url", fun() ->
            assert_mcp_elicitation_context(#{
                <<"mode">> => <<"url">>,
                <<"url">> => <<"https://example.com/elicitation">>,
                <<"elicitationId">> => <<"elicitation">>
            })
        end}
    ].

assert_mcp_elicitation_context(ModeParams) ->
    Params0 = maps:merge(#{
        <<"serverName">> => <<"server">>,
        <<"threadId">> => <<"thread">>,
        <<"turnId">> => <<"turn">>,
        <<"message">> => <<"approve">>
    }, ModeParams),
    Json = #{
        <<"id">> => <<"elicitation-1">>,
        <<"method">> => <<"mcpServer/elicitation/request">>,
        <<"params">> => Params0
    },
    Request = klsn_rule:normalize(
        Json,
        {alias, {coderlx_app_server_rules, server_request}}
    ),
    Params = klsn_rule:normalize(
        maps:get(params, Request),
        {alias, {coderlx_app_server_rules, mcp_server_elicitation_request_params}}
    ),
    ?assertMatch(#{
        serverName := <<"server">>,
        threadId := <<"thread">>,
        turnId := <<"turn">>
    }, Params).
