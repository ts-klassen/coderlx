-ifndef(CODERLX_PARAMETERIZED_RULES_HRL).
-define(CODERLX_PARAMETERIZED_RULES_HRL, true).

-define(JSONRPC_ERROR_OR(ResponseRule), {any_of, [{tuple, {{exact, ok}, {alias, {coderlx_app_server_rules, ResponseRule}}}}, {tuple, {{exact, error}, {alias, {coderlx_app_server_rules, jsonrpc_error}}}}]}).

-endif.
