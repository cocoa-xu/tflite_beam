-module(tflite_beam_utils_downloader).
-export([download/4]).

download(URL, CacheSubdir, CacheFilename, ForceDownload) ->
    case cache_path(CacheSubdir, CacheFilename, ForceDownload) of 
        {ok, Exists, DestionationDir, DestionationFilename} ->
            if 
                Exists ->
                    {ok, DestionationFilename};
                true ->
                    filelib:ensure_dir(DestionationDir),
                    do_download(URL, DestionationFilename)
            end;
        {error, Err} ->
            {error, Err}
    end.

cache_opts() ->
    case os:getenv("MIX_XDG") of
        false ->
            #{};
        _ ->
            #{os => linux}
    end.

maybe_override_by_env(EnvName, Default) ->
    case os:getenv(EnvName) of
        false ->
            Default;
        Value ->
            Value
    end.

mkdir_dir_p(Dir) ->
    %% make_dir creates one level. Every HuggingFace repo id is owner/name and
    %% goes in as the cache subdirectory, so this failed with enoent for all 88
    %% models in the contrib catalogue and download_model never fetched one.
    %% ensure_dir builds the parents of the path it is given, so it needs a name
    %% inside the directory we actually want.
    case filelib:ensure_dir(filename:join(Dir, ".keep")) of
        ok ->
            ok;
        {error, Reason} ->
            {error, lists:flatten(io_lib:fwrite("Cannot create directory ~s: ~s", [Dir, Reason]))}
    end.

cache_basepath() ->
    CacheBaseDir = filename:basedir(user_cache, "tflite_beam", cache_opts()),
    CacheDir = maybe_override_by_env("TFLITE_BEAM_CACHE_DIR", CacheBaseDir),
    case mkdir_dir_p(CacheDir) of
        ok ->
            {ok, CacheDir};
        {error, Reason} ->
            {error, lists:flatten(io_lib:fwrite("Cannot create cache directory ~s: ~s", [CacheDir, Reason]))}
    end.

%% Neither of these may leave the cache directory. filename:join/2 hands back an
%% absolute second argument unchanged, so join(Cache, "/etc/anything") is
%% "/etc/anything", and a relative one may still climb out with "..". Both
%% arrive from the caller: tflite_beam_contrib_huggingface passes a repository
%% name and a filename straight through from whatever asked for the model.
inside_cache(Component) ->
    %% filename:split/1 hands back parts in whatever representation it was
    %% given, so a binary component splits into binaries and lists:member/2
    %% comparing against the string ".." found nothing in it. Every component
    %% arriving from Elixir is a binary, which is to say the check only ever ran
    %% for callers writing Erlang string literals. Flatten first, then compare.
    Flat = flatten_component(Component),
    case filename:pathtype(Flat) =:= relative andalso
         not lists:member("..", filename:split(Flat)) of
        true -> ok;
        false -> {error, lists:flatten(io_lib:fwrite(
                     "~ts would write outside the cache directory", [Flat]))}
    end.

%% A binary that is not UTF-8 is still a filename. Erlang uses raw binaries for
%% exactly that where the filesystem promises no encoding, so refusing them would
%% have turned a check about where a name points into a rule about how it is
%% spelled.
flatten_component(Component) when is_binary(Component) ->
    case unicode:characters_to_list(Component) of
        Flat when is_list(Flat) -> Flat;
        _NotUnicode -> binary_to_list(Component)
    end;
flatten_component(Component) ->
    Component.

cache_path(CacheSubdir, CacheFilename, ForceDownload) ->
    case [E || C <- [CacheSubdir, CacheFilename], {error, E} <- [inside_cache(C)]] of
        [Escape | _] ->
            {error, Escape};
        [] ->
            cache_path_checked(CacheSubdir, CacheFilename, ForceDownload)
    end.

cache_path_checked(CacheSubdir, CacheFilename, ForceDownload) ->
    case cache_basepath() of
        {ok, BaseDir} ->
            case leads_outside(BaseDir, CacheSubdir, CacheFilename) of
                {error, Escape} -> {error, Escape};
                ok -> cache_path_under(BaseDir, CacheSubdir, CacheFilename, ForceDownload)
            end;
        {error, Err} ->
            {error, Err}
    end.

%% inside_cache/1 reads the name; this reads where the name lands. A component
%% with no ".." in it still reaches outside when something along the way is a
%% symlink pointing there, and only resolving against the cache root shows that.
leads_outside(BaseDir, CacheSubdir, CacheFilename) ->
    Relative = filename:join(flatten_component(CacheSubdir),
                             flatten_component(CacheFilename)),
    case filelib:safe_relative_path(Relative, BaseDir) of
        unsafe ->
            {error, lists:flatten(io_lib:fwrite(
                        "~ts would write outside the cache directory", [Relative]))};
        _Safe ->
            ok
    end.

cache_path_under(BaseDir, CacheSubdir, CacheFilename, ForceDownload) ->
            CacheDir = filename:join(BaseDir, CacheSubdir),
            case mkdir_dir_p(CacheDir) of
                ok ->
                    CacheFile = filename:join(CacheDir, CacheFilename),
                    FileExists = if 
                        ForceDownload -> false;
                        true -> filelib:is_file(CacheFile)
                    end,
                    {ok, FileExists,CacheDir,CacheFile};
                _ ->
                    {error, lists:flatten(io_lib:fwrite("Cannot create cache directory ~s", [CacheDir]))}
            end.

certificate_store() ->
    PossibleLocations = [
        %% Configured cacertfile
        os:getenv("TFLITE_BEAM_CACERT"),

        %% Debian/Ubuntu/Gentoo etc.
        "/etc/ssl/certs/ca-certificates.crt",

        %% Fedora/RHEL 6
        "/etc/pki/tls/certs/ca-bundle.crt",

        %5 OpenSUSE
        "/etc/ssl/ca-bundle.pem",

        %% OpenELEC
        "/etc/pki/tls/cacert.pem",

        %% CentOS/RHEL 7
        "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem",

        %% Open SSL on MacOS
        "/usr/local/etc/openssl/cert.pem",

        %% MacOS & Alpine Linux
        "/etc/ssl/cert.pem"
    ],
    CheckExistance = lists:map(fun (F) -> 
        {filelib:is_file(F), F}
    end, PossibleLocations),
    ExistingOnes = lists:dropwhile(fun ({X, _}) -> X == false end, CheckExistance),
    case length(ExistingOnes) of
        Len when Len > 0 ->
            {_, Cert} = hd(ExistingOnes),
            Cert;
        _ ->
            io:fwrite("[WARNING] Cannot find CA certificate store in default locations: ~p~n", [PossibleLocations]),
            io:fwrite("You can set environment variable TFLITE_BEAM_CACERT to the SSL cert on your system.~n"),
            nil
    end.

preferred_ciphers() ->
    PreferredCiphers = [
      %% Cipher suites (TLS 1.3): TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256
      #{cipher => aes_128_gcm, key_exchange => any, mac => aead, prf => sha256},
      #{cipher => aes_256_gcm, key_exchange => any, mac => aead, prf => sha384},
      #{cipher => chacha20_poly1305, key_exchange => any, mac => aead, prf => sha256},
      %% Cipher suites (TLS 1.2): ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:
      %% ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:
      %% ECDHE-RSA-CHACHA20-POLY1305:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384
      #{cipher => aes_128_gcm, key_exchange => ecdhe_ecdsa, mac => aead, prf => sha256},
      #{cipher => aes_128_gcm, key_exchange => ecdhe_rsa, mac => aead, prf => sha256},
      #{cipher => aes_256_gcm, key_exchange => ecdh_ecdsa, mac => aead, prf => sha384},
      #{cipher => aes_256_gcm, key_exchange => ecdh_rsa, mac => aead, prf => sha384},
      #{cipher => chacha20_poly1305, key_exchange => ecdhe_ecdsa, mac => aead, prf => sha256},
      #{cipher => chacha20_poly1305, key_exchange => ecdhe_rsa, mac => aead, prf => sha256},
      #{cipher => aes_128_gcm, key_exchange => dhe_rsa, mac => aead, prf => sha256},
      #{cipher => aes_256_gcm, key_exchange => dhe_rsa, mac => aead, prf => sha384}
    ],
    ssl:filter_cipher_suites(PreferredCiphers, []).

protocol_versions() ->
    case list_to_integer(erlang:system_info(otp_release)) of
        Version when Version < 25 ->
            ['tlsv1.2'];
        _ ->
            ['tlsv1.2', 'tlsv1.3']
    end.

preferred_eccs() ->
    %% TLS curves: X25519, prime256v1, secp384r1
    PreferredECCS = [secp256r1, secp384r1],
    ssl:eccs() -- ssl:eccs() -- PreferredECCS.

secure_ssl() ->
    case os:getenv("TFLITE_BEAM_UNSAFE_HTTPS") of
        nil -> true;
        "FALSE" -> false;
        "false" -> false;
        "nil" -> false;
        "NIL" -> false;
        _ -> true
    end.

https_opts(Hostname) ->
    CertFile = certificate_store(),
    case {secure_ssl(), is_list(CertFile)} of
        {true, true} ->
            [
                {
                    ssl, [
                        {verify, verify_peer},
                        {cacertfile, CertFile},
                        {depth, 4},
                        {ciphers, preferred_ciphers()},
                        {versions, protocol_versions()},
                        {eccs, preferred_eccs()},
                        {reuse_sessions, true},
                        {server_name_indication, Hostname},
                        {secure_renegotiate, true},
                        {customize_hostname_check, [
                            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                        ]}
                    ]
                }
            ];
        _ ->
            [
                {
                    ssl, [
                        {verify, verify_none},
                        {ciphers, preferred_ciphers()},
                        {versions, protocol_versions()},
                        {reuse_sessions, true},
                        {server_name_indication, Hostname},
                        {secure_renegotiate, true}
                    ]
                }
            ]
    end.

set_proxy_if_exists(ProxyEnvVar, ProxyAtom) ->
    case os:getenv(ProxyEnvVar) of
        false ->
            ok;
        ProxyUri ->
            case uri_string:parse(ProxyUri) of
                #{host := Host, port := Port} ->
                    httpc:set_options([{ProxyAtom, {{Host, Port}, []}}]),
                    ok;
                _ ->
                    ok
            end
    end.

do_download(URL, DestionationFilename) ->
    {Parsed, HttpOpts} = case uri_string:parse(URL) of
        #{scheme := "http"} ->
            {ok, nil};
        #{scheme := "https", host := Host} ->
            ssl:start(),
            {ok, https_opts(Host)};
        _ ->
            {error, "Unsupported URL scheme"}
    end,
    case Parsed of
        ok ->
            application:ensure_started(inets),
            set_proxy_if_exists("HTTP_PROXY", proxy),
            set_proxy_if_exists("HTTPS_PROXY", https_proxy),
            set_proxy_if_exists("http_proxy", proxy),
            set_proxy_if_exists("https_proxy", https_proxy),
            Request = {URL, []},
            case httpc:request(get, Request, HttpOpts, [{body_format, binary}]) of
                {ok, {{_, 200, _}, _, Body}} ->
                    case file:write_file(DestionationFilename, Body) of
                        ok ->
                            {ok, DestionationFilename};
                        Err ->
                            {error, lists:flatten(io_lib:fwrite("Cannot write file ~s: ~p", [DestionationFilename, Err]))}
                    end;
                {ok, {{_, StatusCode, _}, _, Body}} ->
                    {error, lists:flatten(io_lib:fwrite("Cannot download file from ~s, status code ~p: ~p", [URL, StatusCode, Body]))};
                {error, Reason} ->
                    {error, Reason};
                Err ->
                    {error, lists:flatten(io_lib:fwrite("Cannot download file from ~s: ~p", [URL, Err]))}
            end;
        error ->
            {error, lists:flatten(io_lib:fwrite("Cannot parse URL ~s", [URL]))}
    end.
