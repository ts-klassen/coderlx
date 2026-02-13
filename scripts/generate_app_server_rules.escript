#!/opt/qrpc/pkg/bin/escript
%%! -noshell

main([SchemaDir, OutPath]) ->
    ok = add_code_paths(),
    SchemaFiles = schema_files(),
    Aliases = lists:append([schema_aliases(SchemaDir, Entry) || Entry <- SchemaFiles]) ++
        codex_event_notification_aliases(SchemaDir),
    ok = write_module(OutPath, Aliases);
main(_) ->
    usage().

usage() ->
    io:format("usage: generate_app_server_rules.escript <schema_dir> <out_path>~n"),
    erlang:halt(1).

add_code_paths() ->
    ScriptPath = escript:script_name(),
    ScriptsDir = filename:dirname(ScriptPath),
    RootDir = filename:dirname(ScriptsDir),
    Patterns = [
        filename:join([RootDir, "_build", "default", "lib", "*", "ebin"]),
        filename:join([RootDir, "_build", "default", "checkouts", "*", "ebin"])
    ],
    EbinDirs = lists:append([filelib:wildcard(Pattern) || Pattern <- Patterns]),
    case EbinDirs of
        [] ->
            io:format("missing build artifacts. run 'rebar3 compile' first.~n"),
            erlang:halt(1);
        _ ->
            lists:foreach(fun code:add_pathz/1, EbinDirs)
    end,
    ok.

schema_files() ->
    [
        {jsonrpc_request, from_json, "JSONRPCRequest.json"}
      , {jsonrpc_notification, from_json, "JSONRPCNotification.json"}
      , {jsonrpc_response, from_json, "JSONRPCResponse.json"}
      , {jsonrpc_error, from_json, "JSONRPCError.json"}
      , {client_request, to_json, "ClientRequest.json"}
      , {client_notification, to_json, "ClientNotification.json"}
      , {server_request, from_json, "ServerRequest.json"}
      , {server_notification, from_json, "ServerNotification.json"}
      , {initialize_params, to_json, "v1/InitializeParams.json"}
      , {initialize_response, from_json, "v1/InitializeResponse.json"}
    ].

schema_aliases(SchemaDir, {AliasName, Direction, FileName})
        when Direction =:= from_json; Direction =:= to_json ->
    Rules = schema_rules(SchemaDir, FileName),
    Rule = maps:get(Direction, Rules),
    [{AliasName, Rule}];
schema_aliases(_SchemaDir, Entry) ->
    io:format("invalid schema entry (expected {AliasName, Direction, FileName}): ~p~n", [Entry]),
    erlang:halt(1).

schema_rules(SchemaDir, FileName) ->
    SchemaPath = filename:join([SchemaDir, FileName]),
    {ok, SchemaBin} = file:read_file(SchemaPath),
    Schema = jsone:decode(SchemaBin),
    klsn_rule_generator:from_json_schema(Schema).

codex_event_notification_aliases(SchemaDir) ->
    EventSchemaPath = filename:join([SchemaDir, "EventMsg.json"]),
    {ok, EventSchemaBin} = file:read_file(EventSchemaPath),
    EventSchema0 = jsone:decode(EventSchemaBin),
    EventTypes = event_types(EventSchema0),
    MethodEnums = [<<"codex/event/", Type/binary>> || Type <- EventTypes],
    Schema = codex_event_notification_schema(EventSchema0, MethodEnums),
    #{from_json := FromRule} = klsn_rule_generator:from_json_schema(Schema),
    [
        {codex_event_notification, FromRule}
        %% {codex_event_notification_to_json, ToRule}
    ].

event_types(EventSchema) ->
    OneOf = maps:get(<<"oneOf">>, EventSchema, []),
    Types = lists:append([event_type_from_entry(Entry) || Entry <- OneOf]),
    lists:usort(Types).

event_type_from_entry(Entry) ->
    Properties = maps:get(<<"properties">>, Entry, #{}),
    TypeProp = maps:get(<<"type">>, Properties, #{}),
    case maps:get(<<"enum">>, TypeProp, []) of
        [] -> [];
        Enums when is_list(Enums) -> Enums
    end.

codex_event_notification_schema(EventSchema0, MethodEnums) ->
    EventSchema = maps:remove(<<"$schema">>, EventSchema0),
    Definitions = maps:get(<<"definitions">>, EventSchema, #{}),
    EventDefinition = maps:remove(<<"definitions">>, EventSchema),
    EventPayload = event_payload_schema(EventDefinition),
    #{
        <<"type">> => <<"object">>,
        <<"required">> => [<<"method">>, <<"params">>],
        <<"properties">> => #{
            <<"method">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => MethodEnums
            },
            <<"params">> => #{
                <<"$ref">> => <<"#/definitions/EventMsgPayload">>
            }
        },
        <<"definitions">> => Definitions#{
            <<"EventMsg">> => EventDefinition,
            <<"EventMsgPayload">> => EventPayload
        }
    }.

event_payload_schema(EventSchema) ->
    OneOf = maps:get(<<"oneOf">>, EventSchema, []),
    PayloadOneOf = [strip_event_type(Entry) || Entry <- OneOf],
    EventSchema#{<<"oneOf">> => PayloadOneOf}.

strip_event_type(Entry) ->
    Properties0 = maps:get(<<"properties">>, Entry, #{}),
    Properties = maps:remove(<<"type">>, Properties0),
    Entry1 = Entry#{<<"properties">> => Properties},
    case maps:get(<<"required">>, Entry1, none) of
        none ->
            Entry1;
        Required0 when is_list(Required0) ->
            Required = [Field || Field <- Required0, Field =/= <<"type">>],
            Entry1#{<<"required">> => Required}
    end.

write_module(OutPath, Aliases) ->
    Header = [
        "%% This file is auto-generated by scripts/generate_app_server_rules.escript.\n",
        "%% Do not edit manually.\n",
        "-module(coderlx_app_server_rules).\n",
        "-klsn_rule_alias([\n"
    ],
    Body = render_aliases(Aliases),
    Footer = "\n]).\n",
    ok = file:write_file(OutPath, [Header, Body, Footer]).

render_aliases([]) ->
    <<>>;
render_aliases([First | Rest]) ->
    [format_alias(First) | lists:map(fun(Alias) ->
        [",\n", format_alias(Alias)]
    end, Rest)].

format_alias({AliasName, Rule}) ->
    io_lib:format("    {~p, ~p}", [AliasName, Rule]).
