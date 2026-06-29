#!/usr/bin/escript
%%! -noshell

main([SchemaDir, OutPath]) ->
    ok = add_code_paths(),
    SchemaFiles = schema_files(),
    Aliases = lists:append([schema_aliases(SchemaDir, Entry) || Entry <- SchemaFiles]),
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
      , {command_execution_request_approval_params, from_json,
            "CommandExecutionRequestApprovalParams.json"}
      , {command_execution_request_approval_response, to_json,
            "CommandExecutionRequestApprovalResponse.json"}
      , {file_change_request_approval_params, from_json,
            "FileChangeRequestApprovalParams.json"}
      , {file_change_request_approval_response, to_json,
            "FileChangeRequestApprovalResponse.json"}
      , {tool_request_user_input_params, from_json,
            "ToolRequestUserInputParams.json"}
      , {tool_request_user_input_response, to_json,
            "ToolRequestUserInputResponse.json"}
      , {mcp_server_elicitation_request_params, from_json,
            "McpServerElicitationRequestParams.json"}
      , {mcp_server_elicitation_request_response, to_json,
            "McpServerElicitationRequestResponse.json"}
      , {permissions_request_approval_params, from_json,
            "PermissionsRequestApprovalParams.json"}
      , {permissions_request_approval_response, to_json,
            "PermissionsRequestApprovalResponse.json"}
      , {dynamic_tool_call_params, from_json, "DynamicToolCallParams.json"}
      , {dynamic_tool_call_response, to_json, "DynamicToolCallResponse.json"}
      , {chatgpt_auth_tokens_refresh_params, from_json,
            "ChatgptAuthTokensRefreshParams.json"}
      , {chatgpt_auth_tokens_refresh_response, to_json,
            "ChatgptAuthTokensRefreshResponse.json"}
      , {attestation_generate_params, from_json, "AttestationGenerateParams.json"}
      , {attestation_generate_response, to_json, "AttestationGenerateResponse.json"}
      , {initialize_params, to_json, "v1/InitializeParams.json"}
      , {initialize_response, from_json, "v1/InitializeResponse.json"}
      , {thread_start_params, to_json, "v2/ThreadStartParams.json"}
      , {thread_start_response, from_json, "v2/ThreadStartResponse.json"}
      , {turn_start_params, to_json, "v2/TurnStartParams.json"}
      , {turn_start_response, from_json, "v2/TurnStartResponse.json"}
      , {turn_steer_params, to_json, "v2/TurnSteerParams.json"}
      , {turn_steer_response, from_json, "v2/TurnSteerResponse.json"}
      , {turn_interrupt_params, to_json, "v2/TurnInterruptParams.json"}
      , {turn_interrupt_response, from_json, "v2/TurnInterruptResponse.json"}
      , {thread_resume_params, to_json, "v2/ThreadResumeParams.json"}
      , {thread_resume_response, from_json, "v2/ThreadResumeResponse.json"}
      , {thread_fork_params, to_json, "v2/ThreadForkParams.json"}
      , {thread_fork_response, from_json, "v2/ThreadForkResponse.json"}
      , {thread_list_params, to_json, "v2/ThreadListParams.json"}
      , {thread_list_response, from_json, "v2/ThreadListResponse.json"}
      , {thread_loaded_list_params, to_json, "v2/ThreadLoadedListParams.json"}
      , {thread_loaded_list_response, from_json, "v2/ThreadLoadedListResponse.json"}
      , {thread_read_params, to_json, "v2/ThreadReadParams.json"}
      , {thread_read_response, from_json, "v2/ThreadReadResponse.json"}
      , {thread_archive_params, to_json, "v2/ThreadArchiveParams.json"}
      , {thread_archive_response, from_json, "v2/ThreadArchiveResponse.json"}
      , {thread_set_name_params, to_json, "v2/ThreadSetNameParams.json"}
      , {thread_set_name_response, from_json, "v2/ThreadSetNameResponse.json"}
      , {thread_unarchive_params, to_json, "v2/ThreadUnarchiveParams.json"}
      , {thread_unarchive_response, from_json, "v2/ThreadUnarchiveResponse.json"}
      , {thread_compact_start_params, to_json, "v2/ThreadCompactStartParams.json"}
      , {thread_compact_start_response, from_json, "v2/ThreadCompactStartResponse.json"}
      , {thread_rollback_params, to_json, "v2/ThreadRollbackParams.json"}
      , {thread_rollback_response, from_json, "v2/ThreadRollbackResponse.json"}
      , {review_start_params, to_json, "v2/ReviewStartParams.json"}
      , {review_start_response, from_json, "v2/ReviewStartResponse.json"}
      , {command_exec_params, to_json, "v2/CommandExecParams.json"}
      , {command_exec_response, from_json, "v2/CommandExecResponse.json"}
      , {model_list_params, to_json, "v2/ModelListParams.json"}
      , {model_list_response, from_json, "v2/ModelListResponse.json"}
      , {skills_list_params, to_json, "v2/SkillsListParams.json"}
      , {skills_list_response, from_json, "v2/SkillsListResponse.json"}
      , {skills_config_write_params, to_json, "v2/SkillsConfigWriteParams.json"}
      , {skills_config_write_response, from_json,
            "v2/SkillsConfigWriteResponse.json"}
      , {apps_list_params, to_json, "v2/AppsListParams.json"}
      , {apps_list_response, from_json, "v2/AppsListResponse.json"}
      , {mcp_server_oauth_login_params, to_json, "v2/McpServerOauthLoginParams.json"}
      , {mcp_server_oauth_login_response, from_json, "v2/McpServerOauthLoginResponse.json"}
      , {mcp_server_status_list_params, to_json, "v2/ListMcpServerStatusParams.json"}
      , {mcp_server_status_list_response, from_json, "v2/ListMcpServerStatusResponse.json"}
      , {config_read_params, to_json, "v2/ConfigReadParams.json"}
      , {config_read_response, from_json, "v2/ConfigReadResponse.json"}
      , {config_batch_write_params, to_json, "v2/ConfigBatchWriteParams.json"}
      , {config_value_write_params, to_json, "v2/ConfigValueWriteParams.json"}
      , {config_write_response, from_json, "v2/ConfigWriteResponse.json"}
      , {config_requirements_read_response, from_json, "v2/ConfigRequirementsReadResponse.json"}
      , {mcp_server_refresh_response, from_json, "v2/McpServerRefreshResponse.json"}
      , {account_read_params, to_json, "v2/GetAccountParams.json"}
      , {account_read_response, from_json, "v2/GetAccountResponse.json"}
      , {account_login_start_params, to_json, "v2/LoginAccountParams.json"}
      , {account_login_start_response, from_json, "v2/LoginAccountResponse.json"}
      , {account_login_cancel_params, to_json, "v2/CancelLoginAccountParams.json"}
      , {account_login_cancel_response, from_json, "v2/CancelLoginAccountResponse.json"}
      , {account_logout_response, from_json, "v2/LogoutAccountResponse.json"}
      , {account_rate_limits_read_response, from_json,
            "v2/GetAccountRateLimitsResponse.json"}
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
    Schema = prepare_schema(jsone:decode(SchemaBin)),
    klsn_rule_generator:from_json_schema(Schema).

prepare_schema(Schema) ->
    merge_mcp_server_elicitation_request_params(Schema).

%% klsn_rule_generator emits oneOf as alternatives and drops sibling object
%% fields, so copy the MCP elicitation context fields into each branch.
%% FIXME: Remove this workaround once klsn handles oneOf sibling keywords:
%% https://github.com/ts-klassen/klsn/issues/49
merge_mcp_server_elicitation_request_params(Schema0) ->
    Schema = case maps:get(<<"title">>, Schema0, undefined) of
        <<"McpServerElicitationRequestParams">> ->
            merge_root_properties_into_one_of(Schema0);
        _ ->
            Schema0
    end,
    update_definition(
        <<"McpServerElicitationRequestParams">>,
        fun merge_root_properties_into_one_of/1,
        Schema
    ).

update_definition(Name, Fun, Schema = #{<<"definitions">> := Definitions}) ->
    case maps:find(Name, Definitions) of
        {ok, Definition} ->
            Schema#{<<"definitions">> := Definitions#{Name := Fun(Definition)}};
        error ->
            Schema
    end;
update_definition(_Name, _Fun, Schema) ->
    Schema.

merge_root_properties_into_one_of(
        Schema = #{<<"oneOf">> := OneOf, <<"properties">> := RootProperties})
        when is_list(OneOf), is_map(RootProperties) ->
    RootRequired = maps:get(<<"required">>, Schema, []),
    MergedOneOf = [
        merge_root_properties_into_branch(RootProperties, RootRequired, Branch)
        || Branch <- OneOf
    ],
    maps:remove(
        <<"required">>,
        maps:remove(<<"properties">>, Schema#{<<"oneOf">> => MergedOneOf})
    );
merge_root_properties_into_one_of(Schema) ->
    Schema.

merge_root_properties_into_branch(
        RootProperties,
        RootRequired,
        Branch = #{<<"properties">> := BranchProperties})
        when is_map(BranchProperties) ->
    BranchRequired = maps:get(<<"required">>, Branch, []),
    Branch#{
        <<"properties">> => maps:merge(RootProperties, BranchProperties),
        <<"required">> => append_unique(RootRequired, BranchRequired)
    };
merge_root_properties_into_branch(_RootProperties, _RootRequired, Branch) ->
    Branch.

append_unique(First, Second) ->
    First ++ [Item || Item <- Second, not lists:member(Item, First)].

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
