%% @doc Loading a delegate out of a shared library at runtime. The cases that
%% need a real plugin use the one in test/plugin, built only when
%% TFLITE_BEAM_BUILD_TEST_PLUGIN is on, and skip when it is not there.
-module(tflite_beam_external_delegate_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    external_is_available/1,
    external_missing_file/1,
    external_not_a_library/1,
    external_wrong_symbols/1,
    external_create_failure/1,
    external_option_cap/1,
    external_option_types/1,
    external_options_round_trip/1,
    external_claims_nothing/1,
    external_prepare_failure/1,
    external_outlives_builder/1
]).

-define(FILLED(V), binary:copy(<<V:32/float-native>>, 1 * 8 * 8 * 3)).
-define(INPUTS, [?FILLED(1.0), ?FILLED(2.0), ?FILLED(3.0), ?FILLED(4.0)]).

all() ->
    [
        external_is_available,
        external_missing_file,
        external_not_a_library,
        external_wrong_symbols,
        external_create_failure,
        external_option_cap,
        external_option_types,
        external_options_round_trip,
        external_claims_nothing,
        external_prepare_failure,
        external_outlives_builder
    ].

%% Loading a plugin needs nothing from TfLite's build options, only the dynamic
%% loader, so this is true on every target including the ones without XNNPACK.
external_is_available(_Config) ->
    ?assert(lists:member(external, tflite_beam_delegate:available())).

external_missing_file(_Config) ->
    {error, Reason} = tflite_beam_delegate:external("/nowhere/libnothing.so"),
    ?assertNotEqual(nomatch, binary:match(Reason, <<"no such delegate library">>)).

%% A real file that is not a library at all: the loader refuses it and says so.
external_not_a_library(_Config) ->
    {error, Reason} = tflite_beam_delegate:external(tflite_beam_test_models:path("add.bin")),
    ?assertNotEqual(nomatch, binary:match(Reason, <<"cannot load delegate library">>)).

%% The case this whole constructor is shaped around. TfLite's own
%% TfLiteExternalDelegateCreate hands back a delegate whose Prepare is
%% indeterminate here, and attaching that takes the VM down; this has to be an
%% ordinary error instead, with no delegate produced to attach.
external_wrong_symbols(_Config) ->
    case library_without_plugin_symbols() of
        false ->
            {skip, "no symbol-less library to point at on this build"};
        Path ->
            Result = tflite_beam_delegate:external(Path),
            ?assertMatch({error, _}, Result),
            {error, Reason} = Result,
            ?assertNotEqual(nomatch, binary:match(Reason, <<"tflite_plugin_create_delegate">>))
    end.

%% A plugin that refuses -- no device present, in the real world -- reports why,
%% and that reason survives to the caller. TfLite discards it by passing nullptr
%% where the plugin expects a report_error callback.
external_create_failure(_Config) ->
    skip_without_plugin(fun(Plugin) ->
        {error, Reason} = tflite_beam_delegate:external(Plugin, #{mode => fail_create}),
        ?assertNotEqual(nomatch, binary:match(Reason, <<"asked to fail at create">>))
    end).

external_option_cap(_Config) ->
    skip_without_plugin(fun(Plugin) ->
        TooMany = maps:from_list([{integer_to_binary(N), <<"v">>} || N <- lists:seq(1, 257)]),
        ?assertMatch({error, _}, tflite_beam_delegate:external(Plugin, TooMany)),
        JustFits = maps:from_list([{integer_to_binary(N), <<"v">>} || N <- lists:seq(1, 256)]),
        ?assertMatch({ok, _}, tflite_beam_delegate:external(Plugin, JustFits))
    end).

external_option_types(_Config) ->
    skip_without_plugin(fun(Plugin) ->
        ?assertMatch({error, _}, tflite_beam_delegate:external(Plugin, #{key => self()}))
    end).

%% Keys and values both have to survive the boundary, and atoms and integers
%% have to arrive as their text, since the plugin ABI carries nothing else.
external_options_round_trip(Config) ->
    skip_without_plugin(fun(Plugin) ->
        Log = filename:join(?config(priv_dir, Config), "options.log"),
        {ok, _} = tflite_beam_delegate:external(Plugin, #{
            <<"log_path">> => Log,
            <<"foo">> => <<"bar">>,
            device => 3
        }),
        {ok, Written} = file:read_file(Log),
        ?assertNotEqual(nomatch, binary:match(Written, <<"foo=bar">>)),
        ?assertNotEqual(nomatch, binary:match(Written, <<"device=3">>))
    end).

%% A delegate that claims no nodes still has to attach, build and invoke without
%% disturbing anything: same output as the plain run, and an execution plan that
%% has not moved.
external_claims_nothing(_Config) ->
    skip_without_plugin(fun(Plugin) ->
        {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
        {ok, Delegate} = tflite_beam_delegate:external(Plugin),
        ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),
        ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
        ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
        ?assertEqual([0, 1, 2], tflite_beam_interpreter:execution_plan(Interpreter)),
        ?assertEqual(3, tflite_beam_interpreter:nodes_size(Interpreter)),
        ?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                     tflite_beam_interpreter:predict(Interpreter, ?INPUTS))
    end).

%% A delegate whose Prepare fails is the real test of build/2 returning the
%% status it was given: before that fix this reported ok and left an empty
%% interpreter behind.
external_prepare_failure(_Config) ->
    skip_without_plugin(fun(Plugin) ->
        {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
        {ok, Delegate} = tflite_beam_delegate:external(Plugin, #{mode => fail_prepare}),
        ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),
        ?assertMatch({error, _}, tflite_beam_interpreter_builder:build(Builder, Interpreter)),
        ?assertMatch({error, _}, tflite_beam_interpreter:nodes_size(Interpreter))
    end).

external_outlives_builder(_Config) ->
    skip_without_plugin(fun(Plugin) ->
        Interpreter = interpreter_from_a_dropped_builder(Plugin),
        erlang:garbage_collect(),
        timer:sleep(100),
        ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
        ?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                     tflite_beam_interpreter:predict(Interpreter, ?INPUTS))
    end).

interpreter_from_a_dropped_builder(Plugin) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    {ok, Delegate} = tflite_beam_delegate:external(Plugin),
    ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    Interpreter.

%% Not priv/tflite_beam.so: we export both plugin symbols ourselves, through
%% libcoral's posenet decoder, so pointing at it succeeds and hands back a
%% posenet delegate.
library_without_plugin_symbols() ->
    Candidates = filelib:wildcard(filename:join([code:lib_dir(tflite_beam), "priv", "libedgetpu", "libusb*"])),
    case [C || C <- Candidates, filelib:is_regular(C)] of
        [Path | _] -> Path;
        [] -> false
    end.

plugin() ->
    Candidates = [
        filename:join([code:lib_dir(tflite_beam), "test", "plugin", "tflite_beam_test_delegate.so"]),
        filename:absname("test/plugin/tflite_beam_test_delegate.so")
    ],
    case [C || C <- Candidates, filelib:is_regular(C)] of
        [Path | _] -> Path;
        [] -> false
    end.

skip_without_plugin(Body) ->
    case plugin() of
        false -> {skip, "test/plugin/tflite_beam_test_delegate.so was not built"};
        Path -> Body(Path)
    end.
