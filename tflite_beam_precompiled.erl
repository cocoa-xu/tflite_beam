-module(tflite_beam_precompiled).
-export([is_precompiled_binary_available/0, install_precompiled_binary_if_available/0]).

-define(PRECOMPILED_TARBALL_NAME, "tflite_beam-nif-~s-~s-v~s").
-define(PRECOMPILED_DOWNLOAD_URL, "https://github.com/cocoa-xu/tflite_beam/releases/download/v~s/~s").
-define(TFLITE_BEAM_SO_FILE, "priv/tflite_beam.so").
-define(TFLITE_BEAM_DLL_FILE, "priv/tflite_beam.dll").
-define(LIBEDGETPU_RUNTIME_VERSION, "0.1.12").

-include_lib("kernel/include/file.hrl").

app_version() ->
    {ok, Cwd} = file:get_cwd(),
    Src = filename:join([Cwd, "src", "tflite_beam.app.src"]),
    case file:read_file(Src) of
        {ok, BinContent} ->
            Content = binary_to_list(BinContent),
            case erl_scan:string(Content) of
                {ok, Tokens, _} ->
                    case erl_parse:parse_term(Tokens) of
                        {ok, {application, tflite_beam, App}} ->
                            case proplists:get_value(vsn, App) of
                                undefined ->
                                    "unknown";
                                Version ->
                                    case is_list(Version) of
                                        true ->
                                            Version;
                                        false ->
                                            "unknown"
                                    end
                            end;
                        {error, _} -> 
                            "unknown"
                    end;
                {error, _} ->
                    "unknown"
            end;
        {error, _} ->
            "unknown"
    end.

is_dev() ->
    AppVersion = app_version(),
    case string:find(AppVersion, "-dev") of
        nomatch ->
            {false, AppVersion};
        _ ->
            {true, AppVersion}
    end.

only_darwin(DarwinABI) ->
    case string:prefix(DarwinABI, "darwin") of
        nomatch ->
            DarwinABI;
        _ ->
            "darwin"
    end.

maybe_override_by_env(EnvName, Default) ->
    case os:getenv(EnvName) of
        false ->
            Default;
        Value ->
            Value
    end.

get_target() ->
    TargetParts = string:split(erlang:system_info(system_architecture), "-", all),
    [G_ARCH, G_OS, G_ABI] = case length(TargetParts) of
        4 ->
            [T_ARCH, _, T_OS, T_ABI] = TargetParts,
            [T_ARCH, T_OS, only_darwin(T_ABI)];
        3 ->
            [T_ARCH, T_OS, T_ABI] = TargetParts,
            [T_ARCH, T_OS, only_darwin(T_ABI)];
        1 ->
            case TargetParts of
                ["win32"] ->
                    WIN_ARCH = case maybe_override_by_env("PROCESSOR_ARCHITECTURE", "x86_64") of
                        "ARM64" ->
                            "aarch64";
                        PROCESSOR_ARCHITECTURE ->
                            PROCESSOR_ARCHITECTURE
                    end,
                    [WIN_ARCH, "windows", "msvc"];
                _ ->
                    ["unknown", "unknown", "unknown"]
            end
    end,
    ARCH = maybe_override_by_env("TARGET_ARCH", G_ARCH),
    CPU = maybe_override_by_env("TARGET_CPU", ""),
    FinalARCH = 
        case ARCH of
            "arm" ->
                CheckString = string:substr(CPU, 1, 7),
                case CheckString of 
                    "cortex_" ->
                        "armv7l";
                    "arm1176" ->
                        "armv6";
                    "arm1156" ->
                        "armv6";
                    "arm1136" ->
                        "armv6";
                    _ ->
                        ARCH
                end;
            _ ->
                ARCH
        end,
    OS = maybe_override_by_env("TARGET_OS", G_OS),
    ABI = maybe_override_by_env("TARGET_ABI", G_ABI),
    TRIPLET = io_lib:fwrite("~s-~s-~s", [FinalARCH, OS, ABI]),
    TRIPLET.

get_nif_version() ->
    erlang:system_info(nif_version).

get_precompiled_nif_version() ->
    "2.16".

is_precompiled_binary_available() ->
    case is_dev() of
        {true, _} ->
            false;
        {false, AppVersion} ->
            Target = get_target(),
            NifVersion = get_nif_version(),
            PrecompiledNifVersion = get_precompiled_nif_version(),
            case NifVersion < PrecompiledNifVersion of
                true ->
                    {false, "NIF version is too low, will fallback to compiling from source code."};
                false ->
                    Name = lists:flatten(io_lib:fwrite(?PRECOMPILED_TARBALL_NAME, [PrecompiledNifVersion, Target, AppVersion])),
                    TarballFilename = lists:flatten(io_lib:fwrite("~s.tar.gz", [Name])),
                    TarballURL = lists:flatten(io_lib:fwrite(?PRECOMPILED_DOWNLOAD_URL, [AppVersion, TarballFilename])),
                    {true, Name, TarballFilename, TarballURL}
            end
    end.

cache_opts() ->
    case os:getenv("MIX_XDG") of
        false ->
            #{};
        _ ->
            #{os => linux}
    end.

cache_path(Filename) ->
    CacheBaseDir = filename:basedir(user_cache, "", cache_opts()),
    CacheDir = maybe_override_by_env("ELIXIR_MAKE_CACHE_DIR", CacheBaseDir),
    file:make_dir(CacheDir),
    Filepath = filename:join([CacheDir, Filename]),
    {filelib:is_file(Filepath), Filepath}.

download_precompiled_binary(URL, CacheTo) ->
    case do_download(URL) of
        {ok, Body} ->
            file:write_file(CacheTo, Body),
            FullPath = CacheTo,
            io:fwrite("[INFO] Precompiled binary tarball downloaded and saved to ~s~n", [FullPath]),
            {ok, FullPath};
        {error, DownloadError} ->
            io:fwrite("[ERROR] Cannot download precompiled binary from ~p: ~p~n", [URL, DownloadError]),
            {error, CacheTo}
    end.

download(URL, CacheFilename) ->
    {Exists, CacheTo} = cache_path(CacheFilename),
    if 
        Exists ->
            io:fwrite("[INFO] Precompiled binary tarball cached at ~s\r\n", [CacheTo]),
            {ok, CacheTo};
        true ->
            io:fwrite("[INFO] not downloaded, will download!~n"),
            download_precompiled_binary(URL, CacheTo)
    end.

certificate_store() ->
    PossibleLocations = [
        %% Configured cacertfile
        os:getenv("ELIXIR_MAKE_CACERT"),

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
            io:fwrite("You can set environment variable ELIXIR_MAKE_CACERT to the SSL cert on your system.~n"),
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
    case os:getenv("ELIXIR_MAKE_UNSAFE_HTTPS") of
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

do_download(URL) ->
    application:ensure_started(inets),
    ssl:start(),
    set_proxy_if_exists("HTTP_PROXY", proxy),
    set_proxy_if_exists("HTTPS_PROXY", https_proxy),
    set_proxy_if_exists("http_proxy", proxy),
    set_proxy_if_exists("https_proxy", https_proxy),
    HttpOtps = https_opts("github.com"),
    Request = {URL, []},
    case httpc:request(get, Request, HttpOtps, [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
          {ok, Body};
        {error, Reason} ->
          {error, Reason};
        Err ->
            {error, lists:flatten(io_lib:fwrite("Cannot download file from ~s: ~p~n", [URL, Err]))}
    end.

is_already_installed() ->
    case string:find(get_target(), "windows-msvc") of
        nomatch ->
            filelib:is_regular(?TFLITE_BEAM_SO_FILE);
        _ ->
            filelib:is_regular(?TFLITE_BEAM_DLL_FILE)
    end.

install_precompiled_binary_if_available() ->
    case is_already_installed() of
        false ->
            case is_precompiled_binary_available() of
                {true, Name, TarballFilename, TarballURL} ->
                    case download(TarballURL, TarballFilename) of
                        {error, _} ->
                            exit(failed);
                        {ok, TarballFileFullPath} ->
                            file:del_dir_r("tmp_priv"),
                            Status = 
                                case erl_tar:extract(TarballFileFullPath, [compressed, {cwd, "tmp_priv"}]) of
                                    ok ->
                                        file:del_dir_r("priv"),
                                        TmpPriv = filename:join(["tmp_priv", Name, "priv"]),
                                        
                                        PrivRenameOk = file:rename(TmpPriv, "priv"),
                                        case PrivRenameOk of
                                            {error, PrivError} ->
                                                io:fwrite("[ERROR] Failed to move priv directory: ~p~n", [PrivError]),
                                                failed;
                                            _ ->
                                                ok
                                        end;
                                    Error ->
                                        io:fwrite("[ERROR] Failed to unarchive tarball file: ~s, error: ~p~n", [TarballFileFullPath, Error]),
                                        failed
                                end,
                            file:del_dir_r("tmp_priv"),
                            case Status of 
                                failed ->
                                    exit(failed);
                                _ ->
                                    ok
                            end
                    end;
                What ->
                    io:fwrite("[INFO] Cannot find precompiled binary: ~p~n", [What]),
                    exit(failed)
            end;
        true ->
            ok
    end.
