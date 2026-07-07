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
      , {thread_delete_params, to_json, "v2/ThreadDeleteParams.json"}
      , {thread_delete_response, from_json, "v2/ThreadDeleteResponse.json"}
      , {thread_unsubscribe_params, to_json, "v2/ThreadUnsubscribeParams.json"}
      , {thread_unsubscribe_response, from_json, "v2/ThreadUnsubscribeResponse.json"}
      , {thread_goal_set_params, to_json, "v2/ThreadGoalSetParams.json"}
      , {thread_goal_set_response, from_json, "v2/ThreadGoalSetResponse.json"}
      , {thread_goal_get_params, to_json, "v2/ThreadGoalGetParams.json"}
      , {thread_goal_get_response, from_json, "v2/ThreadGoalGetResponse.json"}
      , {thread_goal_clear_params, to_json, "v2/ThreadGoalClearParams.json"}
      , {thread_goal_clear_response, from_json, "v2/ThreadGoalClearResponse.json"}
      , {thread_metadata_update_params, to_json, "v2/ThreadMetadataUpdateParams.json"}
      , {thread_metadata_update_response, from_json, "v2/ThreadMetadataUpdateResponse.json"}
      , {thread_shell_command_params, to_json, "v2/ThreadShellCommandParams.json"}
      , {thread_shell_command_response, from_json, "v2/ThreadShellCommandResponse.json"}
      , {thread_approve_guardian_denied_action_params, to_json,
            "v2/ThreadApproveGuardianDeniedActionParams.json"}
      , {thread_approve_guardian_denied_action_response, from_json,
            "v2/ThreadApproveGuardianDeniedActionResponse.json"}
      , {thread_inject_items_params, to_json, "v2/ThreadInjectItemsParams.json"}
      , {thread_inject_items_response, from_json, "v2/ThreadInjectItemsResponse.json"}
      , {review_start_params, to_json, "v2/ReviewStartParams.json"}
      , {review_start_response, from_json, "v2/ReviewStartResponse.json"}
      , {command_exec_params, to_json, "v2/CommandExecParams.json"}
      , {command_exec_response, from_json, "v2/CommandExecResponse.json"}
      , {command_exec_write_params, to_json, "v2/CommandExecWriteParams.json"}
      , {command_exec_write_response, from_json, "v2/CommandExecWriteResponse.json"}
      , {command_exec_terminate_params, to_json, "v2/CommandExecTerminateParams.json"}
      , {command_exec_terminate_response, from_json, "v2/CommandExecTerminateResponse.json"}
      , {command_exec_resize_params, to_json, "v2/CommandExecResizeParams.json"}
      , {command_exec_resize_response, from_json, "v2/CommandExecResizeResponse.json"}
      , {model_list_params, to_json, "v2/ModelListParams.json"}
      , {model_list_response, from_json, "v2/ModelListResponse.json"}
      , {model_provider_capabilities_read_params, to_json,
            "v2/ModelProviderCapabilitiesReadParams.json"}
      , {model_provider_capabilities_read_response, from_json,
            "v2/ModelProviderCapabilitiesReadResponse.json"}
      , {skills_list_params, to_json, "v2/SkillsListParams.json"}
      , {skills_list_response, from_json, "v2/SkillsListResponse.json"}
      , {skills_config_write_params, to_json, "v2/SkillsConfigWriteParams.json"}
      , {skills_config_write_response, from_json,
            "v2/SkillsConfigWriteResponse.json"}
      , {skills_extra_roots_set_params, to_json, "v2/SkillsExtraRootsSetParams.json"}
      , {skills_extra_roots_set_response, from_json,
            "v2/SkillsExtraRootsSetResponse.json"}
      , {apps_list_params, to_json, "v2/AppsListParams.json"}
      , {apps_list_response, from_json, "v2/AppsListResponse.json"}
      , {mcp_server_oauth_login_params, to_json, "v2/McpServerOauthLoginParams.json"}
      , {mcp_server_oauth_login_response, from_json, "v2/McpServerOauthLoginResponse.json"}
      , {mcp_server_status_list_params, to_json, "v2/ListMcpServerStatusParams.json"}
      , {mcp_server_status_list_response, from_json, "v2/ListMcpServerStatusResponse.json"}
      , {mcp_server_resource_read_params, to_json, "v2/McpResourceReadParams.json"}
      , {mcp_server_resource_read_response, from_json, "v2/McpResourceReadResponse.json"}
      , {mcp_server_tool_call_params, to_json, "v2/McpServerToolCallParams.json"}
      , {mcp_server_tool_call_response, from_json, "v2/McpServerToolCallResponse.json"}
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
      , {account_rate_limit_reset_credit_consume_params, to_json,
            "v2/ConsumeAccountRateLimitResetCreditParams.json"}
      , {account_rate_limit_reset_credit_consume_response, from_json,
            "v2/ConsumeAccountRateLimitResetCreditResponse.json"}
      , {account_usage_read_response, from_json, "v2/GetAccountTokenUsageResponse.json"}
      , {account_workspace_messages_read_response, from_json,
            "v2/GetWorkspaceMessagesResponse.json"}
      , {account_send_add_credits_nudge_email_params, to_json,
            "v2/SendAddCreditsNudgeEmailParams.json"}
      , {account_send_add_credits_nudge_email_response, from_json,
            "v2/SendAddCreditsNudgeEmailResponse.json"}
      , {hooks_list_params, to_json, "v2/HooksListParams.json"}
      , {hooks_list_response, from_json, "v2/HooksListResponse.json"}
      , {marketplace_add_params, to_json, "v2/MarketplaceAddParams.json"}
      , {marketplace_add_response, from_json, "v2/MarketplaceAddResponse.json"}
      , {marketplace_remove_params, to_json, "v2/MarketplaceRemoveParams.json"}
      , {marketplace_remove_response, from_json, "v2/MarketplaceRemoveResponse.json"}
      , {marketplace_upgrade_params, to_json, "v2/MarketplaceUpgradeParams.json"}
      , {marketplace_upgrade_response, from_json, "v2/MarketplaceUpgradeResponse.json"}
      , {plugin_list_params, to_json, "v2/PluginListParams.json"}
      , {plugin_list_response, from_json, "v2/PluginListResponse.json"}
      , {plugin_installed_params, to_json, "v2/PluginInstalledParams.json"}
      , {plugin_installed_response, from_json, "v2/PluginInstalledResponse.json"}
      , {plugin_read_params, to_json, "v2/PluginReadParams.json"}
      , {plugin_read_response, from_json, "v2/PluginReadResponse.json"}
      , {plugin_skill_read_params, to_json, "v2/PluginSkillReadParams.json"}
      , {plugin_skill_read_response, from_json, "v2/PluginSkillReadResponse.json"}
      , {plugin_share_save_params, to_json, "v2/PluginShareSaveParams.json"}
      , {plugin_share_save_response, from_json, "v2/PluginShareSaveResponse.json"}
      , {plugin_share_update_targets_params, to_json,
            "v2/PluginShareUpdateTargetsParams.json"}
      , {plugin_share_update_targets_response, from_json,
            "v2/PluginShareUpdateTargetsResponse.json"}
      , {plugin_share_list_params, to_json, "v2/PluginShareListParams.json"}
      , {plugin_share_list_response, from_json, "v2/PluginShareListResponse.json"}
      , {plugin_share_checkout_params, to_json, "v2/PluginShareCheckoutParams.json"}
      , {plugin_share_checkout_response, from_json, "v2/PluginShareCheckoutResponse.json"}
      , {plugin_share_delete_params, to_json, "v2/PluginShareDeleteParams.json"}
      , {plugin_share_delete_response, from_json, "v2/PluginShareDeleteResponse.json"}
      , {plugin_install_params, to_json, "v2/PluginInstallParams.json"}
      , {plugin_install_response, from_json, "v2/PluginInstallResponse.json"}
      , {plugin_uninstall_params, to_json, "v2/PluginUninstallParams.json"}
      , {plugin_uninstall_response, from_json, "v2/PluginUninstallResponse.json"}
      , {fs_read_file_params, to_json, "v2/FsReadFileParams.json"}
      , {fs_read_file_response, from_json, "v2/FsReadFileResponse.json"}
      , {fs_write_file_params, to_json, "v2/FsWriteFileParams.json"}
      , {fs_write_file_response, from_json, "v2/FsWriteFileResponse.json"}
      , {fs_create_directory_params, to_json, "v2/FsCreateDirectoryParams.json"}
      , {fs_create_directory_response, from_json, "v2/FsCreateDirectoryResponse.json"}
      , {fs_get_metadata_params, to_json, "v2/FsGetMetadataParams.json"}
      , {fs_get_metadata_response, from_json, "v2/FsGetMetadataResponse.json"}
      , {fs_read_directory_params, to_json, "v2/FsReadDirectoryParams.json"}
      , {fs_read_directory_response, from_json, "v2/FsReadDirectoryResponse.json"}
      , {fs_remove_params, to_json, "v2/FsRemoveParams.json"}
      , {fs_remove_response, from_json, "v2/FsRemoveResponse.json"}
      , {fs_copy_params, to_json, "v2/FsCopyParams.json"}
      , {fs_copy_response, from_json, "v2/FsCopyResponse.json"}
      , {fs_watch_params, to_json, "v2/FsWatchParams.json"}
      , {fs_watch_response, from_json, "v2/FsWatchResponse.json"}
      , {fs_unwatch_params, to_json, "v2/FsUnwatchParams.json"}
      , {fs_unwatch_response, from_json, "v2/FsUnwatchResponse.json"}
      , {experimental_feature_list_params, to_json,
            "v2/ExperimentalFeatureListParams.json"}
      , {experimental_feature_list_response, from_json,
            "v2/ExperimentalFeatureListResponse.json"}
      , {experimental_feature_enablement_set_params, to_json,
            "v2/ExperimentalFeatureEnablementSetParams.json"}
      , {experimental_feature_enablement_set_response, from_json,
            "v2/ExperimentalFeatureEnablementSetResponse.json"}
      , {permission_profile_list_params, to_json, "v2/PermissionProfileListParams.json"}
      , {permission_profile_list_response, from_json, "v2/PermissionProfileListResponse.json"}
      , {windows_sandbox_setup_start_params, to_json,
            "v2/WindowsSandboxSetupStartParams.json"}
      , {windows_sandbox_setup_start_response, from_json,
            "v2/WindowsSandboxSetupStartResponse.json"}
      , {windows_sandbox_readiness_response, from_json,
            "v2/WindowsSandboxReadinessResponse.json"}
      , {external_agent_config_detect_params, to_json,
            "v2/ExternalAgentConfigDetectParams.json"}
      , {external_agent_config_detect_response, from_json,
            "v2/ExternalAgentConfigDetectResponse.json"}
      , {external_agent_config_import_params, to_json,
            "v2/ExternalAgentConfigImportParams.json"}
      , {external_agent_config_import_response, from_json,
            "v2/ExternalAgentConfigImportResponse.json"}
      , {external_agent_config_import_read_histories_response, from_json,
            "v2/ExternalAgentConfigImportHistoriesReadResponse.json"}
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
