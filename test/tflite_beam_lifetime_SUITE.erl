%% @doc What stays alive, what stops being usable, and when.
%%
%% Every case here stands for a defect that reached the repository. A NIF fault
%% takes the emulator with it, so the ones that used to crash cannot be left to a
%% reviewer's eye. Where a handle is meant to survive, the case uses it; where it
%% is meant to be refused, the case asserts the refusal rather than the absence
%% of a crash, since a crash would take the run down and never reach an assert.
-module(tflite_beam_lifetime_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../src/tflite_beam/tflite_beam_records.hrl").

-export([all/0]).
-export([
    tensor_handle_retired_by_allocate/1,
    tensor_handle_retired_by_resize/1,
    tensor_handle_usable_before_retirement/1,
    signature_runner_retired_by_rebuild/1,
    signature_runner_usable_before_rebuild/1,
    signature_runner_registry_does_not_grow/1,
    model_outlives_its_error_reporter_handle/1,
    error_reporter_shared_between_models/1,
    concurrent_runner_fetch_and_rebuild/1,
    concurrent_resize_and_invoke/1
]).

all() ->
    [
        tensor_handle_retired_by_allocate,
        tensor_handle_retired_by_resize,
        tensor_handle_usable_before_retirement,
        signature_runner_retired_by_rebuild,
        signature_runner_usable_before_rebuild,
        signature_runner_registry_does_not_grow,
        model_outlives_its_error_reporter_handle,
        error_reporter_shared_between_models,
        concurrent_runner_fetch_and_rebuild,
        concurrent_resize_and_invoke
    ].

%% An interpreter over Name, built and allocated, plus its first input index.
ready(Name) ->
    Interpreter = tflite_beam_test_models:interpreter(Name),
    {ok, [Index | _]} = tflite_beam_interpreter:inputs(Interpreter),
    {Interpreter, Index}.

%% A builder and an interpreter that has been built into once already.
rebuildable(Name) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder(Name),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    {Builder, Interpreter}.

is_retired({error, Reason}) -> binary:match(Reason, <<"has been">>) =/= nomatch;
is_retired(_) -> false.

%% AllocateTensors can move every TfLiteTensor, so a handle taken before it is
%% no longer pointing at its tensor. It used to keep working: set_data wrote
%% through it at the old size and to_binary read the bytes back.
tensor_handle_retired_by_allocate(_Config) ->
    {Interpreter, Index} = ready("add.bin"),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, Index),
    ?assert(is_binary(tflite_beam_tensor:to_binary(Tensor))),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    ?assert(is_retired(tflite_beam_tensor:to_binary(Tensor))),
    ?assert(is_retired(tflite_beam_tensor:set_data(Tensor, <<0:32>>))).

%% Same for a reshape, which is the shorter route to the same invalidation.
tensor_handle_retired_by_resize(_Config) ->
    {Interpreter, Index} = ready("dynamic_shapes.bin"),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, Index),
    ok = tflite_beam_interpreter:resize_input_tensor(Interpreter, Index, [2, 2, 3]),
    ?assert(is_retired(tflite_beam_tensor:to_binary(Tensor))).

%% Retirement has to be the exception: a handle fetched after the last reshape
%% is the normal way to use this API and must work, at the new size.
tensor_handle_usable_before_retirement(_Config) ->
    {Interpreter, Index} = ready("dynamic_shapes.bin"),
    ok = tflite_beam_interpreter:resize_input_tensor(Interpreter, Index, [2, 2, 3]),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    Tensor = tflite_beam_interpreter:tensor(Interpreter, Index),
    Bytes = 2 * 2 * 3 * 4,
    ok = tflite_beam_tensor:set_data(Tensor, binary:copy(<<7>>, Bytes)),
    ?assertEqual(binary:copy(<<7>>, Bytes), tflite_beam_tensor:to_binary(Tensor)).

%% Building into an interpreter destroys the one a runner borrows from. The
%% runner resource survives, so nothing tells it, and it used to read freed
%% memory: heap-use-after-free in signature_key, freed by ~Interpreter.
signature_runner_retired_by_rebuild(_Config) ->
    {Builder, Interpreter} = rebuildable("add.bin"),
    {ok, Runner} = tflite_beam_interpreter:get_signature_runner(Interpreter, nil),
    ?assertMatch({ok, _}, tflite_beam_signature_runner:signature_key(Runner)),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ?assert(is_retired(tflite_beam_signature_runner:signature_key(Runner))),
    ?assert(is_retired(tflite_beam_signature_runner:input_size(Runner))).

signature_runner_usable_before_rebuild(_Config) ->
    {_Builder, Interpreter} = rebuildable("add.bin"),
    {ok, Runner} = tflite_beam_interpreter:get_signature_runner(Interpreter, nil),
    ?assertMatch({ok, _}, tflite_beam_signature_runner:signature_key(Runner)),
    ?assertMatch({ok, _}, tflite_beam_signature_runner:input_size(Runner)).

%% The registry that makes the retirement above possible must not hold the
%% runners it tracks. Unlike the rest of this suite this one guards against a
%% mistake made while fixing the others rather than one that predates them: the
%% first version of the registry took a reference per runner, so an interpreter
%% outliving the runners taken from it accumulated all of them, about 120 bytes
%% each. It cannot fail against a build that has no registry at all.
signature_runner_registry_does_not_grow(_Config) ->
    {_Builder, Interpreter} = rebuildable("add.bin"),
    Fetch = fun(N) ->
        [begin
            {ok, _} = tflite_beam_interpreter:get_signature_runner(Interpreter, nil)
         end || _ <- lists:seq(1, N)],
        erlang:garbage_collect(),
        erlang:memory(binary)
    end,
    _Warmup = Fetch(2000),
    Before = Fetch(2000),
    After = Fetch(20000),
    %% ten times the fetches must not mean ten times the memory. The bug grew by
    %% about 120 bytes each, so twenty thousand of them showed up as megabytes.
    ?assert(After - Before < 1024 * 1024,
            lists:flatten(io_lib:format("registry grew by ~p bytes", [After - Before]))).

%% TFLite keeps the reporter pointer for the model's lifetime, so the resource
%% behind it has to outlive the model regardless of what Erlang still holds.
model_outlives_its_error_reporter_handle(_Config) ->
    Reporter = tflite_beam_error_reporter:default_error_reporter(),
    Model = tflite_beam_flatbuffer_model:build_from_file(
              tflite_beam_test_models:path("add.bin"), [{error_reporter, Reporter}]),
    #tflite_beam_flatbuffer_model{ref = Ref} = Model,
    erlang:garbage_collect(),
    ?assertEqual(true, tflite_beam_flatbuffer_model:initialized(Ref)).

%% And one reporter may back two models, which the API allows by accepting it as
%% an option, so neither model may assume it owns it.
error_reporter_shared_between_models(_Config) ->
    First = tflite_beam_flatbuffer_model:build_from_file(tflite_beam_test_models:path("add.bin")),
    #tflite_beam_flatbuffer_model{ref = FirstRef} = First,
    Reporter = tflite_beam_flatbuffer_model:error_reporter(FirstRef),
    Second = tflite_beam_flatbuffer_model:build_from_file(
               tflite_beam_test_models:path("multi_add.bin"), [{error_reporter, Reporter}]),
    #tflite_beam_flatbuffer_model{ref = SecondRef} = Second,
    erlang:garbage_collect(),
    ?assertEqual(true, tflite_beam_flatbuffer_model:initialized(FirstRef)),
    ?assertEqual(true, tflite_beam_flatbuffer_model:initialized(SecondRef)).

%% GetSignatureRunner reads as a lookup and writes interpreter state, so it
%% raced a rebuild on another scheduler: SEGV inside
%% CreatePlaceholderSignatureDef. Both sides now take the interpreter's guard.
concurrent_runner_fetch_and_rebuild(_Config) ->
    {Builder, Interpreter} = rebuildable("add.bin"),
    Parent = self(),
    Fetchers = [spawn_link(fun() ->
        [catch tflite_beam_interpreter:get_signature_runner(Interpreter, nil)
         || _ <- lists:seq(1, 2000)],
        Parent ! {done, self()}
    end) || _ <- lists:seq(1, 4)],
    Rebuilder = spawn_link(fun() ->
        [catch tflite_beam_interpreter_builder:build(Builder, Interpreter)
         || _ <- lists:seq(1, 200)],
        Parent ! {done, self()}
    end),
    [receive {done, Pid} -> ok after 120000 -> ct:fail({timeout, Pid}) end
     || Pid <- [Rebuilder | Fetchers]],
    %% surviving is the assertion: the old code aborted the node here
    ?assert(is_integer(tflite_beam_interpreter:tensors_size(Interpreter))).

%% ResizeInputTensor mutates and took no guard, so two of them at once freed the
%% same dims array: POINTER_BEING_FREED_WAS_NOT_ALLOCATED in ResizeTensorImpl.
concurrent_resize_and_invoke(_Config) ->
    {Interpreter, Index} = ready("dynamic_shapes.bin"),
    Parent = self(),
    Resizers = [spawn_link(fun() ->
        %% shapes wide enough that the reshape reallocates rather than fitting
        %% in place, which is what opens the window two of these used to race in
        [catch tflite_beam_interpreter:resize_input_tensor(
                 Interpreter, Index, [1, 8 + (N rem 16), 8, 3])
         || N <- lists:seq(1, 1500)],
        Parent ! {done, self()}
    end) || _ <- lists:seq(1, 4)],
    Invoker = spawn_link(fun() ->
        [begin
            catch tflite_beam_interpreter:allocate_tensors(Interpreter),
            catch tflite_beam_interpreter:invoke(Interpreter)
         end || _ <- lists:seq(1, 400)],
        Parent ! {done, self()}
    end),
    [receive {done, Pid} -> ok after 120000 -> ct:fail({timeout, Pid}) end
     || Pid <- [Invoker | Resizers]],
    ?assert(is_integer(tflite_beam_interpreter:tensors_size(Interpreter))).
