%% @doc What `tflite_beam_interpreter_builder:build/2' does when it fails, and
%% what it leaves behind when it is called twice. Every case here comes in a
%% pair with one that pins the success path, so that a fix which simply refuses
%% everything cannot pass.
-module(tflite_beam_build_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../src/tflite_beam/tflite_beam_records.hrl").

-export([all/0]).
-export([
    build_failure_returns_error/1,
    build_success_still_returns_ok/1,
    failed_build_leaves_no_usable_interpreter/1,
    live_interpreter_still_accessible/1,
    build_twice_invalidates_tensors/1,
    build_twice_yields_working_tensors/1,
    failed_build_after_tensor_fetch/1
]).

-define(FILLED(V), binary:copy(<<V:32/float-native>>, 1 * 8 * 8 * 3)).

all() ->
    [
        build_failure_returns_error,
        build_success_still_returns_ok,
        failed_build_leaves_no_usable_interpreter,
        live_interpreter_still_accessible,
        build_twice_invalidates_tensors,
        build_twice_yields_working_tensors,
        failed_build_after_tensor_fetch
    ].

%% 0_subgraphs.bin loads as a model and then fails to build, which is the whole
%% reason it is a fixture.
build_failure_returns_error(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("0_subgraphs.bin"),
    ?assertMatch({error, _}, tflite_beam_interpreter_builder:build(Builder, Interpreter)).

build_success_still_returns_ok(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    ?assertEqual(ok, tflite_beam_interpreter_builder:build(Builder, Interpreter)),
    ?assertEqual(ok, tflite_beam_interpreter:allocate_tensors(Interpreter)),
    ?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                 tflite_beam_interpreter:predict(
                     Interpreter, [?FILLED(1.0), ?FILLED(2.0), ?FILLED(3.0), ?FILLED(4.0)])).

%% A failed build empties the interpreter it was building into. Reaching into
%% one of those used to take the whole VM down with SIGSEGV.
failed_build_leaves_no_usable_interpreter(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("0_subgraphs.bin"),
    {error, _} = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ?assertMatch({error, _}, tflite_beam_interpreter:nodes_size(Interpreter)),
    ?assertMatch({error, _}, tflite_beam_interpreter:tensors_size(Interpreter)),
    ?assertMatch({error, _}, tflite_beam_interpreter:execution_plan(Interpreter)),
    ?assertMatch({error, _}, tflite_beam_interpreter:inputs(Interpreter)),
    ?assertMatch({error, _}, tflite_beam_interpreter:allocate_tensors(Interpreter)),
    ?assertMatch({error, _}, tflite_beam_interpreter:tensor(Interpreter, 0)).

%% The other half: the guard has to reject an empty interpreter, not every one.
live_interpreter_still_accessible(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ?assertEqual(7, tflite_beam_interpreter:tensors_size(Interpreter)),
    ?assertEqual({ok, [0, 1, 2, 3]}, tflite_beam_interpreter:inputs(Interpreter)),
    ?assertEqual({ok, [5, 6]}, tflite_beam_interpreter:outputs(Interpreter)),
    ?assert(is_integer(tflite_beam_interpreter:nodes_size(Interpreter))),
    ?assert(is_list(tflite_beam_interpreter:execution_plan(Interpreter))),
    ?assertMatch(#tflite_beam_tensor{}, tflite_beam_interpreter:tensor(Interpreter, 0)).

%% Building again destroys the interpreter the cached tensors were taken from,
%% so every handle handed out before it has to stop working.
build_twice_invalidates_tensors(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, 0),
    ?assert(is_binary(tflite_beam_tensor:to_binary(Tensor))),

    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    {error, Reason} = tflite_beam_tensor:to_binary(Tensor),
    ?assertNotEqual(nomatch, binary:match(Reason, <<"has been dropped">>)),
    ?assertMatch({error, _}, tflite_beam_tensor:dims(Tensor#tflite_beam_tensor.ref)),
    ?assertMatch({error, _}, tflite_beam_tensor:set_data(Tensor, ?FILLED(1.0))).

%% And the other half again: the flush must empty the cache, not break it.
build_twice_yields_working_tensors(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    _Stale = tflite_beam_interpreter:tensor(Interpreter, 0),

    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    Fresh = tflite_beam_interpreter:tensor(Interpreter, 0),
    ?assertMatch(#tflite_beam_tensor{name = <<"a">>, index = 0}, Fresh),
    ok = tflite_beam_tensor:set_data(Fresh, ?FILLED(1.0)),
    ?assertEqual(?FILLED(1.0), tflite_beam_tensor:to_binary(Fresh)),
    ?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                 tflite_beam_interpreter:predict(
                     Interpreter, [?FILLED(1.0), ?FILLED(2.0), ?FILLED(3.0), ?FILLED(4.0)])).

%% The interpreter is destroyed on the way in, before the build can fail, so a
%% failed build invalidates the cache exactly like a successful one.
failed_build_after_tensor_fetch(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, 0),
    ?assert(is_binary(tflite_beam_tensor:to_binary(Tensor))),

    {FailingBuilder, _} = tflite_beam_test_models:builder("0_subgraphs.bin"),
    ?assertMatch({error, _}, tflite_beam_interpreter_builder:build(FailingBuilder, Interpreter)),
    ?assertMatch({error, _}, tflite_beam_tensor:to_binary(Tensor)),
    ?assertMatch({error, _}, tflite_beam_interpreter:nodes_size(Interpreter)).
