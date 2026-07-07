# Codex バージョン更新手順

この手順は、ユーザーが Codex バージョン更新を明示的に依頼した場合だけ使う。通常の調査、実装、レビュー、テスト、またはリポジトリを開いただけの状態では、Codex バージョン更新を開始しない。

このリポジトリでは Codex app-server のバイナリと、その Codex バージョンから生成した JSON Schema 由来の Erlang ルールを固定している。Codex バージョンを更新する場合は、バイナリだけでなく schema 生成物も必ず更新する。

## 重要な制約

- `_build` は rebar3 の作業ディレクトリなので、修正・コミットしない。
- `priv/codex` は取得された Codex バイナリで `.gitignore` 対象なので、通常コミットしない。
- `klsn` ライブラリ自体の修正が必要になった場合は、このリポジトリ内で無理に完遂しない。失敗内容、該当 schema、必要そうな `klsn` 側変更を人間に報告して中断する。
- このリポジトリ内の修正だけで済む場合のみ、更新、検証、差分整理まで完遂する。
- ユーザーの未コミット変更がある場合は上書きしない。

## 通常の更新対象

通常コミット対象になる可能性があるファイルは次の通り。

- `CODEX_VERSION`
- `CODEX_TARBALL_SHA256`
- `CODEX_BINARY_SHA256`
- `src/coderlx_app_server_rules.erl`
- `LICENSE-CODEX`
- `NOTICE-CODEX`
- 必要な場合のみ `src/*.erl`
- `klsn` の tag 更新だけで済む場合のみ `rebar.config` と `rebar.lock`

## 更新手順

1. 更新先の Codex バージョンを決める。
   - GitHub release の `rust-v<version>` に対応する `<version>` を使う。
   - 例: `0.99.0`

2. 既存状態を確認する。
   ```bash
   cat CODEX_VERSION
   git status --short
   ```

3. 更新スクリプトを実行する。
   ```bash
   bash scripts/update_codex_app_server.sh <version>
   ```

   このスクリプトは次を行う。
   - Codex tarball を取得する。
   - tarball と binary の SHA256 を更新する。
   - `priv/codex` を更新する。
   - `codex app-server generate-json-schema --out ...` を実行する。
   - `scripts/generate_app_server_rules.escript` で `src/coderlx_app_server_rules.erl` を再生成する。
   - `LICENSE-CODEX` と `NOTICE-CODEX` を更新する。

4. schema 生成で失敗した場合は原因を分類する。
   - `scripts/generate_app_server_rules.escript` の `schema_files()` に足りない schema があるだけなら、このリポジトリ内で alias の追加・削除・名前変更に追従してよい。
   - `klsn_rule_generator:from_json_schema/1` が新しい JSON Schema 形式を扱えない場合は、`klsn` 側修正が必要な可能性が高い。この場合は作業を止め、人間に報告する。
   - 報告には、失敗した schema ファイル名、エラー全文、該当する JSON Schema の構造、必要そうな `klsn` 側対応を含める。

5. 手書き wrapper の追従を確認する。
   - `src/coderlx.erl` の `server_request_method()`, `server_request_param()`, `server_request_response()`, `server_request_rule_/1` を確認する。
   - 新しい server request method が追加されていれば、このリポジトリ内で対応する。
   - `src/coderlx_thread.erl`, `src/coderlx_turn.erl`, `src/coderlx_config.erl`, `src/coderlx_account.erl`, `src/coderlx_model.erl`, `src/coderlx_skills.erl`, `src/coderlx_mcp.erl`, `src/coderlx_apps.erl` の client request method と生成 alias が一致するか確認する。
   - Codex 側で method 名が変わっている場合は wrapper も修正する。

6. 検証する。
   ```bash
   rebar3 compile
   rebar3 ct
   ```

   認証、ネットワーク、サンドボックス状態に依存するテストで失敗した場合は、compile 結果と失敗理由を分けて報告する。

7. 差分を確認する。
   ```bash
   git diff --stat
   git diff
   ```

   `_build` と `priv/codex` は差分対象にしない。

## 新しい client request method が追加された場合

`b533012`, `03fa3d8`, `7ebbff0` と同じように、次の 3 つに分ける。

1. `b533012`: `scripts/generate_app_server_rules.escript` だけを変更する。
   - `schema_files()` に新しい method の schema alias を追加する。
   - params は `to_json`、response は `from_json`。
   - params schema が無い method は response だけ追加する。

2. `03fa3d8`: `src/coderlx_app_server_rules.erl` だけを再生成する。

3. `7ebbff0`: client request wrapper だけを追加する。
   - 既存 namespace は既存 module に関数を追加する。
   - 新規 namespace は `src/coderlx_<namespace>.erl` を作り、`src/coderlx.app.src` に追加する。
   - params schema が無い method の `-klsn_input_rule` は `{exact, null}`。

## 完了条件

- `rebar3 compile` が成功している。
- `src/coderlx_app_server_rules.erl` が新 Codex の JSON Schema から再生成されている。
- 必要な手書き wrapper 修正が完了している。
- `klsn` 側修正が不要である。
- 実行できる範囲のテスト結果を人間に報告できる。
