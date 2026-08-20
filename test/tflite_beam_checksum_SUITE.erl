%% @doc The check that a downloaded precompiled tarball is the one we published.
%%
%% tflite_beam_precompiled is not part of the application -- the Makefile compiles
%% it on its own, before anything else exists -- so the suite compiles it here and
%% reaches the internals directly rather than adding exports for the test's sake.
-module(tflite_beam_checksum_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1]).
-export([
    manifest_lists_every_target/1,
    accepts_a_tarball_that_matches/1,
    rejects_and_deletes_a_tarball_that_does_not/1,
    rejects_a_file_the_manifest_does_not_list/1
]).

all() ->
    [
        manifest_lists_every_target,
        accepts_a_tarball_that_matches,
        rejects_and_deletes_a_tarball_that_does_not,
        rejects_a_file_the_manifest_does_not_list
    ].

init_per_suite(Config) ->
    Root = repository_root(),
    Source = filename:join(Root, "tflite_beam_precompiled.erl"),
    {ok, Module, Binary} = compile:file(Source, [binary, export_all, nowarn_export_all]),
    {module, Module} = code:load_binary(Module, Source, Binary),
    [{manifest, filename:join(Root, "checksum.term")} | Config].

%% Seven precompiled targets ship; a manifest that lists six would verify six and
%% silently wave the seventh through.
manifest_lists_every_target(Config) ->
    {ok, Entries} = tflite_beam_precompiled:checksums(?config(manifest, Config)),
    ?assertEqual(7, length(Entries)),
    [?assert(is_list(Name) andalso length(Digest) =:= 64) || {Name, Digest} <- Entries].

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

%% A name the manifest says nothing about is refused rather than accepted for
%% lack of an opinion.
rejects_a_file_the_manifest_does_not_list(Config) ->
    {Path, _} = a_file_with_known_digest(Config, "unlisted.tar.gz"),
    ?assertMatch({error, _},
                 tflite_beam_precompiled:verify_checksum(
                     "tflite_beam-nif-2.16-not-a-target-v9.9.9.tar.gz", Path, ?config(manifest, Config))).

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
