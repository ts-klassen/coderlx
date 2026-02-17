-module(coderlx_opts_builder).

-export([
        sandbox/1
    ]).

-spec sandbox(#{
        user => klsn:binstr()
      , codex_home => klsn:binstr()
      , sandbox_home_bind => klsn:binstr()
      , sandbox_work_bind => klsn:binstr()
    }) -> coderlx:opts().
sandbox(Args) ->
    User = case Args of
        #{user := User0} ->
            User0;
        _ ->
            case os:getenv("USER") of
                false ->
                    case os:getenv("LOGNAME") of
                        false ->
                            <<"codex">>;
                        User0 ->
                            klsn_binstr:from_any(User0)
                    end;
                User0 ->
                    klsn_binstr:from_any(User0)
            end
    end,
    CodexHome = case Args of
        #{codex_home := CodexHome0} ->
            CodexHome0;
        _ ->
            case os:getenv("CODEX_HOME") of
                false ->
                    case os:getenv("HOME") of
                        false ->
                            <<"/home/", User/binary, "/.codex">>;
                        Home ->
                            <<(klsn_binstr:from_any(Home))/binary, "/.codex">>
                    end;
                CodexHome0 ->
                    klsn_binstr:from_any(CodexHome0)
            end
    end,
    SandboxHome = case Args of
        #{sandbox_home_bind := HostHome} ->
            {bind, HostHome, <<"/home/", User/binary>>};
        _ ->
            {dir, <<"/home/", User/binary>>}
    end,
    {SandboxWork, Chdir} = case Args of
        #{sandbox_work_bind := HostWork} ->
            Name = filename:basename(HostWork),
            SandboxWork0 = <<"/home/", User/binary, "/work/", Name/binary>>,
            {{bind, HostWork, SandboxWork0}, {chdir, SandboxWork0}};
        _ ->
            SandboxWork0 = <<"/home/", User/binary, "/work/sandbox">>,
            {{dir, SandboxWork0}, {chdir, SandboxWork0}}
    end,
    #{
        bwrap => [
            share_net
          , new_session
          , die_with_parent
          , {tmpfs, <<"/">>}
          , {proc, <<"/proc">>}
          , {dev, <<"/dev">>}
          , {dir, <<"/tmp">>}
          , {ro_bind, <<"/usr">>, <<"/usr">>}
          , {ro_bind, <<"/bin">>, <<"/bin">>}
          , {ro_bind, <<"/lib">>, <<"/lib">>}
          , {ro_bind, <<"/lib64">>, <<"/lib64">>}
          , {ro_bind, <<"/etc">>, <<"/etc">>}
          , {ro_bind, <<"/run">>, <<"/run">>}
          , {ro_bind, <<"/sys">>, <<"/sys">>}
          , {dir, <<"/cbin">>}
          , {ro_bind, coderlx:default_codex_path(), <<"/cbin/codex">>}
          , {dir, <<"/home">>}
          , SandboxHome
          , {bind, CodexHome, <<"/home/", User/binary, "/.codex">>}
          , {tmpfs, <<"/home/", User/binary, "/work">>}
          , SandboxWork
          , clearenv
          , {setenv, <<"HOME">>, <<"/home/", User/binary>>}
          , {setenv, <<"USER">>, User}
          , {setenv, <<"LOGNAME">>, User}
          , {setenv, <<"PATH">>, <<"/cbin:/usr/bin:/bin">>}
          , {setenv, <<"SHELL">>, <<"/bin/bash">>}
          , {setenv, <<"TMPDIR">>, <<"/tmp">>}
          , Chdir
        ]
      , codex_path => <<"/cbin/codex">>
    }.
