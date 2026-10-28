# Codex バージョン更新手順

この手順は、ユーザーが Codex バージョン更新を
明示的に依頼した場合だけ使う。
通常の調査、実装、レビュー、テスト、
またはリポジトリを開いただけの状態では、
Codex バージョン更新を開始しない。

このリポジトリでは Codex app-server と code-mode host のバイナリ、
およびその Codex バージョンから生成した
JSON Schema 由来の Erlang ルールを固定している。
Codex バージョンを更新する場合は、
バイナリだけでなく schema 生成物も必ず更新する。

具体的なバージョンを与えられていない場合は、最新安定版を選ぶ。

codex について言及されていたとしても、
それはこのリポジトリが使っている codex の話である場合は
openai-docs スキルを使うべきではない。むやみにスキルを使わないこと。

## 重要な制約

- `_build` は rebar3 の作業ディレクトリなので、修正・コミットしない。
- `priv/codex-runtime/` と `priv/.codex-*/` は
  取得された Codex パッケージと一時ファイルで
  `.gitignore` 対象なので、通常コミットしない。
- `klsn` ライブラリ自体の修正が必要になった場合は、
  このリポジトリ内で無理に完遂しない。
  失敗内容、該当 schema、必要そうな `klsn` 側変更を人間に報告して中断する。
- このリポジトリ内の修正だけで済む場合のみ、更新、検証、差分整理まで完遂する。
- ユーザーの未コミット変更がある場合は上書きしない。

## 通常の更新対象

通常コミット対象になる可能性があるファイルは次の通り。

- `CODEX_VERSION`
- `scripts/codex/CODEX_RUNTIME_TARBALL_SHA256`
- `scripts/codex/CODEX_RUNTIME_FILES_SHA256`
- `scripts/codex/CODEX_SCHEMA_CLI_TARBALL_SHA256`
- `src/coderlx_app_server_rules.erl`
- `src/coderlx.app.src`
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
   - 実行用の `codex-app-server-package-<target>.tar.gz` と、
     同一版のスキーマ生成用 `codex-<target>.tar.gz` を取得する。
   - 両方の tarball と実行用パッケージ内の全ファイルの SHA256 を更新する。
   - 相対配置を維持して `priv/codex-runtime/` を更新する。
   - 専用 app-server ではスキーマを生成できないため、一時取得した CLI で
     `codex app-server generate-json-schema --out ...` を実行する。
   - `scripts/generate_app_server_rules.escript` で
     `src/coderlx_app_server_rules.erl` を再生成する。
   - `LICENSE-CODEX` と `NOTICE-CODEX` を更新する。

4. schema 生成で失敗した場合は原因を分類する。
   - `scripts/generate_app_server_rules.escript` の `schema_files()` に
     足りない schema があるだけなら、このリポジトリ内で
     alias の追加・削除・名前変更に追従してよい。
   - `klsn_rule_generator:from_json_schema/1` が
     新しい JSON Schema 形式を扱えない場合は、
     `klsn` 側修正が必要な可能性が高い。
     この場合は作業を止め、人間に報告する。
   - 報告には、失敗した schema ファイル名、エラー全文、
     該当する JSON Schema の構造、必要そうな `klsn` 側対応を含める。

5. schema の追加・削除・変更に合わせ、
   関連する手書き型・`-spec`・rule alias・wrapper・dispatch を更新する。
   - `src/coderlx.erl` の `client_request_method()`,
     `server_request_method()`, `server_request_param()`,
     `server_request_response()`, `server_request_rule_/1` を確認する。
   - 新しい server request method が追加されていれば、
     このリポジトリ内で対応する。
   - すべての client request wrapper について、
     method と生成 alias が一致するか確認する。
   - Codex 側で method 名が変わっている場合は wrapper も修正する。

6. 検証する。
   ```bash
   rebar3 compile
   rebar3 eunit
   rebar3 ct
   ```

   認証、ネットワーク、サンドボックス状態に依存するテストで失敗した場合は、
   compile 結果と失敗理由を分けて報告する。

7. 差分を確認する。
   ```bash
   git diff --stat
   git diff
   ```

   `_build`, `priv/codex-runtime/`, `priv/.codex-*/` は差分対象にしない。

## 新しい client request method が追加された場合

`fca1f4a`, `29d9487`, `4e47bbd`, `17766f7` と同じように、
次の 4 つに分けてコミットする。
この後、下記のリリース手順でアプリのバージョン更新を別途コミットする。

1. `fca1f4a`: バージョン固定とライセンス情報の更新をコミットする。
   - `CODEX_VERSION` と `scripts/codex/` 内の 3 つの SHA256 ファイルを更新する。
   - `LICENSE-CODEX` と `NOTICE-CODEX` に差分があれば、このコミットに含める。

2. `29d9487`: `scripts/generate_app_server_rules.escript` だけを変更する。
   - `schema_files()` に新しい method の schema alias を追加する。
   - params は `to_json`、response は `from_json`。
   - params schema が無い method は response だけ追加する。

3. `4e47bbd`: `src/coderlx_app_server_rules.erl` だけを再生成する。

4. `17766f7`: client request wrapper と
   関連する手書き型・`-spec`・rule alias を更新する。
   - 既存 namespace は既存 module に関数を追加する。
   - 新規 namespace は `src/coderlx_<namespace>.erl` を作り、
     `src/coderlx.app.src` に追加する。
   - params schema が無い method の `-klsn_input_rule` は `{exact, null}`。

## リリース手順

更新内容を上記の粒度でコミットし、Gerritにレビューを出した後に進む。
以下の例は`0.165.0`への更新であり、別の版では目的のバージョンに置き換える。

1. `src/coderlx.app.src`の`vsn`を`CODEX_VERSION`と同じ値に更新する。
   `0.165.0`の場合は`{vsn, "0.165.0"}`とする。
   既存の更新コミットに続けて、この`vsn`の変更だけをコミットする。
   コミット名は`Bump version to <version>`とし、今回の例では
   `Bump version to 0.165.0`となる。
   独立したChange-Idを付け、単独のChangeSetとしてGerritにレビューを出す。

2. バージョン更新を含むすべてのChangeSetがマージされたことを確認する。
   マージ済みのバージョン更新コミットのハッシュを記録し、
   ts-klassenに、そのコミットへGerrit画面上からタグを付けるよう依頼する。
   依頼にはタグ名とコミットハッシュを含める。
   タグ名は`0.165.0`のようにバージョン番号だけとし、`v`を付けない。
   `v0.165.0`になっていたら、ts-klassenに付け直すよう依頼する。

3. Gerritのタグが正しいコミットを指していることを確認する。

   ```bash
   git ls-remote --tags origin \
     'refs/tags/0.165.0' 'refs/tags/0.165.0^{}'
   ```

   注釈付きタグで`^{}`の行がある場合は、そのコミットハッシュを使う。
   軽量タグの場合はタグの行のハッシュを使う。
   手順2で記録したハッシュと一致することを確認する。
   一致しなければ、ts-klassenにGerrit画面上での修正を依頼し、再確認する。

4. 正しい位置にタグが付いたことを確認したら、次を実行する。

   ```bash
   qrexec-client-vm github user.Push+coderlx
   ```

   qrexecのpolicyは`ask`なので、時間がたっても進まない場合は
   バックグラウンドで実行し、ユーザーに承認を求める。
   その際は、qrexecのpolicyによるGitHubへのpush承認待ちだと説明する。
   実行中の処理を重複して起動せず、承認後に終了コードと出力を確認する。

5. pushが成功したら、GitHub側のタグとコミットハッシュを確認する。

   ```bash
   git ls-remote --tags https://github.com/ts-klassen/coderlx.git \
     'refs/tags/0.165.0' 'refs/tags/0.165.0^{}'
   ```

   `github.com/ts-klassen/coderlx`に目的のタグが反映され、
   手順3と同じ方法で得たコミットハッシュがGerrit側と一致することを確認する。
   この確認を終えた時点で、アップデートのリリース完了とする。

## 完了条件

- 更新内容とアプリのバージョン更新が、指定された粒度でコミットされている。
- `rebar3 compile` が成功している。
- `src/coderlx_app_server_rules.erl` が
  新 Codex の JSON Schema から再生成されている。
- schema に対応する手書き定義の更新が完了している。
- `klsn` 側修正が不要である。
- 実行できる範囲のテスト結果を人間に報告できる。
- `src/coderlx.app.src`の`vsn`が`CODEX_VERSION`と一致している。
- バージョン更新を含むすべてのChangeSetがGerritでマージされている。
- ts-klassenがGerrit画面上で、マージ済みのバージョン更新コミットに
  `v`のないタグを付け、その位置を確認できている。
- `qrexec-client-vm github user.Push+coderlx`によるpushが成功している。
- GitHub側に同じタグが存在し、Gerrit側とコミットハッシュが一致している。
