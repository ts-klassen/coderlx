-module(coderlx_app_server_rules_tests).

-include_lib("eunit/include/eunit.hrl").

image_reference_test_() ->
    [
        {Name, fun() ->
            Image = Reference#{type => image, detail => high},
            Params = #{threadId => <<"thread">>, input => [Image]},
            ?assertEqual(Params, normalize(Params, turn_start_params)),
            ?assertMatch({reject, _}, klsn_rule:eval(
                Params#{input := [maps:remove(type, Image)]},
                {alias, {coderlx_app_server_rules, turn_start_params}}, #{}))
        end}
        || {Name, Reference} <- [
            {"URL image", #{url => <<"https://example.com/image.png">>}},
            {"file ID image", #{fileId => <<"file-image">>}}
        ]
    ].

turn_disabled_plugins_test() ->
    Params = #{threadId => <<"thread">>, input => [],
               disabledPluginIds => [<<"plugin">>]},
    ?assertEqual(Params, normalize(Params, turn_start_params)).

thread_item_anchor_test() ->
    Params = #{threadId => <<"thread">>, turnId => <<"turn">>,
               cursor => #{type => item, itemId => <<"item">>}},
    ?assertEqual(Params, normalize(Params, thread_items_list_params)).

attachment_notification_test() ->
    Notification = #{method => 'thread/attachment/updated', params => #{
        threadId => <<"thread">>, attachmentId => <<"attachment">>,
        attachmentType => <<"file">>, identityKey => <<"key">>,
        operation => created
    }},
    Json = jsone:decode(jsone:encode(Notification)),
    ?assertEqual(Notification, normalize(Json, server_notification)).

gateway_notification_test() ->
    Notification = #{method => 'account/gatewayOAuth/changed', params => #{
        providerId => <<"provider">>, status => started,
        authUrl => <<"https://example.com/login">>
    }},
    Json = jsone:decode(jsone:encode(Notification)),
    ?assertEqual(Notification, normalize(Json, server_notification)).

normalize(Value, Alias) ->
    klsn_rule:normalize(Value, {alias, {coderlx_app_server_rules, Alias}}).

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
