%% @doc The check that a downloaded precompiled tarball is the one we published.
%%
%% tflite_beam_precompiled is not part of the application -- the Makefile compiles
%% it on its own, before anything else exists -- so the suite compiles it here and
%% reaches the internals directly rather than adding exports for the test's sake.
-module(tflite_beam_checksum_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    manifest_lists_every_target/1,
    accepts_a_tarball_that_matches/1,
    rejects_and_deletes_a_tarball_that_does_not/1,
    rejects_a_file_the_manifest_does_not_list/1,
    a_listed_file_with_a_wrong_digest_is_still_refused/1
]).

all() ->
    [
        manifest_lists_every_target,
        accepts_a_tarball_that_matches,
        rejects_and_deletes_a_tarball_that_does_not,
        rejects_a_file_the_manifest_does_not_list,
        a_listed_file_with_a_wrong_digest_is_still_refused
    ].

init_per_suite(Config) ->
    Root = repository_root(),
    Source = filename:join(Root, "tflite_beam_precompiled.erl"),
    {ok, Module, Binary} = compile:file(Source, [binary, export_all, nowarn_export_all]),
    {module, Module} = code:load_binary(Module, Source, Binary),
    [{manifest, filename:join(Root, "checksum.term")} | Config].

%% init_per_suite replaces the loaded tflite_beam_precompiled with one compiled
%% from source; put the released module back so later suites do not inherit it.
%% Common Test on OTP 28 calls this whether or not it is exported, so a suite
%% with init_per_suite and no end_per_suite fails there while passing on 26.
end_per_suite(Config) ->
    code:purge(tflite_beam_precompiled),
    code:delete(tflite_beam_precompiled),
    Config.

%% The manifest is generated between tagging and publishing and is not tracked, so
%% a checkout usually has none -- and a checkout with none is a documented state,
%% not a broken one. When there is one, though, every published tarball has to be
%% in it: a manifest listing eleven of twelve verifies eleven and waves the
%% twelfth through.
%%
%% Twelve, not seven, since 1.0.0-rc3: seven targets ship a plain tarball and
%% five of them ship a second one with the LiteRT API compiled in. Counting is
%% not enough on its own, so the shape is checked too. armv6 and armv7l build
%% with XNNPACK off and LiteRT's CPU accelerator is XNNPACK, so those two have no
%% LiteRT variant and their presence here would mean the matrix built something
%% that cannot load.
manifest_lists_every_target(Config) ->
    case tflite_beam_precompiled:checksums(?config(manifest, Config)) of
        no_manifest ->
            {skip, "no checksum.term here; it is generated at release time"};
        {ok, Entries} ->
            ?assertEqual(12, length(Entries)),
            {LiteRt, Plain} = lists:partition(
                fun({Name, _}) -> string:find(Name, "-litert-") =/= nomatch end, Entries),
            ?assertEqual(7, length(Plain)),
            ?assertEqual(5, length(LiteRt)),
            [?assertEqual(nomatch, string:find(Name, Arch),
                          lists:flatten(io_lib:format(
                              "~ts has a LiteRT variant, but ~ts builds with XNNPACK off",
                              [Name, Arch])))
             || {Name, _} <- LiteRt, Arch <- ["armv6", "armv7l"]],
            [?assert(is_list(Name) andalso length(Digest) =:= 64) || {Name, Digest} <- Entries],
            %% and every one of them for the version being published. A manifest
            %% left over from the previous release lists seven perfectly good
            %% checksums for seven files nobody is about to download, and the
            %% installer then refuses every target. That is how 0.4.0-rc4 shipped.
            %% not tflite_beam_precompiled:app_version/0: it reads app.src
            %% relative to the working directory, and Common Test runs from its
            %% own log directory, where it finds nothing and answers "unknown"
            Version = version_from(repository_root()),
            Suffix = "-v" ++ Version ++ ".tar.gz",
            [?assert(lists:suffix(Suffix, Name),
                     lists:flatten(io_lib:format("~ts is not a v~ts tarball", [Name, Version])))
             || {Name, _} <- Entries]
    end.

accepts_a_tarball_that_matches(Config) ->
    {Path, Digest} = a_file_with_known_digest(Config, "matching.tar.gz"),
    ?assertEqual({ok, Path}, tflite_beam_precompiled:compare_checksum("matching.tar.gz", Path, Digest)),
    ?assert(filelib:is_regular(Path)).

%% And it has to be deleted, or every later run finds the bad file cached and
%% fails identically with no way out.
rejects_and_deletes_a_tarball_that_does_not(Config) ->
    {Path, Digest} = a_file_with_known_digest(Config, "tampered.tar.gz"),
    Wrong = lists:duplicate(64, $a),
    ?assertNotEqual(Digest, Wrong),
    ?assertMatch({error, _}, tflite_beam_precompiled:compare_checksum("tampered.tar.gz", Path, Wrong)),
    ?assertNot(filelib:is_regular(Path)).

%% A name the manifest says nothing about is refused rather than accepted for lack
%% of an opinion, and it has to stay that way. The only route to this branch is a
%% manifest that does not describe this release, and accepting on that basis would
%% disable verification for every target of the release at once, handing a free
%% pass to anyone who noticed the manifest was stale. That is strictly worse than
%% refusing to install, which is loud, local and recoverable with one environment
%% variable. The release slip that made this look tempting -- 0.4.0-rc4 shipping
%% with rc3's manifest -- is fixed where it happened instead.
rejects_a_file_the_manifest_does_not_list(Config) ->
    {Path, Digest} = a_file_with_known_digest(Config, "unlisted.tar.gz"),
    Manifest = filename:join(?config(priv_dir, Config), "checksum.term"),
    ok = file:write_file(Manifest,
                         io_lib:format("{\"something-else.tar.gz\", \"~s\"}.~n", [Digest])),
    ?assertMatch({error, _},
                 tflite_beam_precompiled:verify_checksum("unlisted.tar.gz", Path, Manifest)),
    %% and the same manifest does accept the name it does list
    ?assertMatch({ok, _},
                 tflite_beam_precompiled:verify_checksum("something-else.tar.gz", Path, Manifest)).

%% The half that must not have been relaxed with it: an entry that exists and
%% disagrees is still a refusal, and the file still goes.
a_listed_file_with_a_wrong_digest_is_still_refused(Config) ->
    {Path, _Digest} = a_file_with_known_digest(Config, "listed-but-wrong.tar.gz"),
    Manifest = filename:join(?config(priv_dir, Config), "checksum.term"),
    ok = file:write_file(Manifest,
                         io_lib:format("{\"listed-but-wrong.tar.gz\", \"~s\"}.~n",
                                       [lists:duplicate(64, $a)])),
    ?assertMatch({error, _},
                 tflite_beam_precompiled:verify_checksum("listed-but-wrong.tar.gz", Path, Manifest)),
    ?assertNot(filelib:is_regular(Path)).

version_from(Root) ->
    {ok, Bin} = file:read_file(filename:join([Root, "src", "tflite_beam.app.src"])),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Bin)),
    {ok, {application, tflite_beam, App}} = erl_parse:parse_term(Tokens),
    proplists:get_value(vsn, App).

a_file_with_known_digest(Config, Name) ->
    Path = filename:join(?config(priv_dir, Config), Name),
    Content = <<"not really a tarball, but it hashes like one">>,
    ok = file:write_file(Path, Content),
    Digest = string:lowercase(binary_to_list(binary:encode_hex(crypto:hash(sha256, Content)))),
    {Path, Digest}.

%% Common Test runs from its own log directory, so the repository is found
%% through the application rather than through the working directory:
%% code:lib_dir gives <root>/_build/<profile>/lib/tflite_beam.
repository_root() ->
    Candidates = [
        filename:join(lists:duplicate(4, "..")),
        "."
    ],
    Roots = [filename:join(code:lib_dir(tflite_beam), C) || C <- Candidates],
    case [R || R <- Roots, filelib:is_regular(filename:join(R, "tflite_beam_precompiled.erl"))] of
        [Root | _] -> Root;
        [] -> error({cannot_find_repository_root, Roots})
    end.
