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

-export([all/0, init_per_testcase/2]).
-export([
    tensor_handle_retired_by_allocate/1,
    tensor_handle_retired_by_resize/1,
    tensor_handle_usable_before_retirement/1,
    tensor_handle_keeps_its_interpreter_alive/1,
    tensor_registry_does_not_grow/1,
    signature_runner_retired_by_rebuild/1,
    signature_runner_usable_before_rebuild/1,
    signature_runner_registry_does_not_grow/1,
    model_outlives_its_error_reporter_handle/1,
    error_reporter_shared_between_models/1,
    concurrent_runner_fetch_and_rebuild/1,
    concurrent_resize_and_invoke/1,
    runner_registry_failure_frees_the_runner_and_the_lock/1,
    interpreter_allocation_failure_is_reported_not_leaked/1,
    builder_allocation_failure_is_reported_not_leaked/1,
    add_delegate_failure_strands_no_delegate/1,
    delegate_transfer_failure_leaves_the_interpreter_untouched/1
]).

all() ->
    [
        tensor_handle_retired_by_allocate,
        tensor_handle_retired_by_resize,
        tensor_handle_usable_before_retirement,
        tensor_handle_keeps_its_interpreter_alive,
        tensor_registry_does_not_grow,
        signature_runner_retired_by_rebuild,
        signature_runner_usable_before_rebuild,
        signature_runner_registry_does_not_grow,
        model_outlives_its_error_reporter_handle,
        error_reporter_shared_between_models,
        concurrent_runner_fetch_and_rebuild,
        concurrent_resize_and_invoke,
        runner_registry_failure_frees_the_runner_and_the_lock,
        interpreter_allocation_failure_is_reported_not_leaked,
        builder_allocation_failure_is_reported_not_leaked,
        add_delegate_failure_strands_no_delegate,
        delegate_transfer_failure_leaves_the_interpreter_untouched
    ].

%% Arming a fault point is a global switch that crosses processes, so the NIF
%% refuses unless TFLITE_BEAM_ENABLE_FAULT_INJECTION is in the environment.
%% os:putenv cannot supply it: since OTP 24 the VM keeps its own table rather
%% than calling setenv, which is not thread-safe, so the C side never sees it. It
%% has to be there before the VM starts, which is what CI does and what a bare
%% `rebar3 ct` does not.
init_per_testcase(Case, Config) when
        Case =:= runner_registry_failure_frees_the_runner_and_the_lock;
        Case =:= interpreter_allocation_failure_is_reported_not_leaked;
        Case =:= builder_allocation_failure_is_reported_not_leaked;
        Case =:= add_delegate_failure_strands_no_delegate;
        Case =:= delegate_transfer_failure_leaves_the_interpreter_untouched ->
    case tflite_beam_nif:nif_arm_fault(none) of
        ok -> Config;
        {error, _} ->
            {skip, "set TFLITE_BEAM_ENABLE_FAULT_INJECTION=1 before starting the VM"}
    end;
init_per_testcase(_Case, Config) ->
    Config.

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

%% A handle borrows a pointer into its interpreter's arena, so it has to keep
%% that interpreter alive. Nothing in Erlang says when it stops doing so on its
%% own: the compiler stops counting a variable as live at its last mention, so an
%% interpreter someone fetched a tensor from and then never named again is
%% collectable while the tensor taken out of it is still in use. Reading through
%% the handle then reads a freed arena. It took a dirty scheduler to make this
%% show up reliably, because the hop onto one is where the collection fits.
tensor_handle_keeps_its_interpreter_alive(_Config) ->
    Tensor = fetch_and_forget("add.bin"),
    erlang:garbage_collect(),
    timer:sleep(50),
    erlang:garbage_collect(),
    ?assert(is_binary(tflite_beam_tensor:to_binary(Tensor))).

%% The interpreter is named for the last time inside here, which is the whole
%% point: on return there is no reference to it left anywhere except the one the
%% handle is supposed to be holding.
fetch_and_forget(Name) ->
    Interpreter = tflite_beam_test_models:interpreter(Name),
    {ok, [Index | _]} = tflite_beam_interpreter:inputs(Interpreter),
    tflite_beam_interpreter:tensor(Interpreter, Index).

%% The registry that makes retirement possible must not hold the handles it
%% tracks, for the same reason the runner one must not: a handle that keeps its
%% interpreter alive and an interpreter that keeps its handles alive is a loop
%% neither end escapes. Like the runner case this guards the fix rather than the
%% defect, and cannot fail against a build that has no registry.
tensor_registry_does_not_grow(_Config) ->
    {Interpreter, Index} = ready("add.bin"),
    Fetch = fun(N) ->
        [tflite_beam_interpreter:tensor(Interpreter, Index) || _ <- lists:seq(1, N)],
        erlang:garbage_collect(),
        erlang:memory(binary) + erlang:memory(system)
    end,
    _Warmup = Fetch(2000),
    Before = Fetch(2000),
    After = Fetch(20000),
    ?assert(After - Before < 1024 * 1024,
            lists:flatten(io_lib:format("registry grew by ~p bytes", [After - Before]))).

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
%%
%% Neither of the next two can fail today, and it is worth saying why rather than
%% leaving them looking stronger than they are. The only reporter this API can
%% hand out is TFLite's default, which is a function-local static that the
%% destructor declines to delete, so a premature release of the resource would
%% not free anything and initialized/1 does not read through the pointer anyway.
%% What they do establish is the shape the fix has to keep: a model built with a
%% reporter stays usable after every Erlang handle to that reporter is gone, and
%% one reporter may back two models. The day a custom reporter can be
%% constructed, these become real, and they are here so that day does not have
%% to remember to add them.
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

%% The rest of this suite reaches its defects by calling the API. The ones below
%% cannot be: they live between the line that takes a reference and the line that
%% records it, and the only thing that separates those is an allocation failing.
%% A test cannot ask a 64 GB machine to run out of memory at one exact line, so
%% the NIF carries named points that can be armed to fail instead. One shot each;
%% see c_src/fault_inject.hpp.
arm(Point) -> ok = tflite_beam_nif:nif_arm_fault(Point).
disarm() -> ok = tflite_beam_nif:nif_arm_fault(none).

is_oom({error, Reason}) -> Reason =:= <<"out of memory">>;
is_oom(_) -> false.

grew_by(Fun) ->
    Fun(), erlang:garbage_collect(),
    Before = erlang:memory(binary) + erlang:memory(system),
    Fun(), erlang:garbage_collect(),
    erlang:memory(binary) + erlang:memory(system) - Before.

%% Failing to grow the registry used to leave three things behind: the registry
%% mutex locked, so every later reader of it waited forever; the runner resource
%% itself, which no term named and no destructor would ever reach; and the
%% reference that runner had taken on its interpreter. The lock is what the
%% second call proves, and the resource is what the loop proves.
runner_registry_failure_frees_the_runner_and_the_lock(_Config) ->
    {_Builder, Interpreter} = rebuildable("add.bin"),
    arm(runner_registry),
    ?assert(is_oom(tflite_beam_interpreter:get_signature_runner(Interpreter, nil))),
    %% The lock came back, so the next caller is served rather than parked.
    %% Against a build without the fix this line does not return and the run
    %% stops here rather than failing: the process is inside a NIF waiting on a
    %% mutex nobody will ever unlock, which no timeout in Erlang can reach, and
    %% the scheduler thread it was running on cannot deliver one either.
    ?assertMatch({ok, _}, tflite_beam_interpreter:get_signature_runner(Interpreter, nil)),
    Fetch = fun() ->
        [begin
            arm(runner_registry),
            catch tflite_beam_interpreter:get_signature_runner(Interpreter, nil)
         end || _ <- lists:seq(1, 20000)]
    end,
    Growth = grew_by(Fetch),
    disarm(),
    ?assert(Growth < 1024 * 1024,
            lists:flatten(io_lib:format("~p bytes left behind by 20000 failures", [Growth]))).

%% An interpreter is a resource plus four containers and two mutexes. Failing
%% part way used to leave the resource allocated with nothing naming it, which is
%% not a leak any garbage collector can find: enif_alloc_resource hands back a
%% reference, and until a term takes it, giving it back is the only way out.
interpreter_allocation_failure_is_reported_not_leaked(_Config) ->
    arm(interpreter_containers),
    ?assertMatch({error, _}, tflite_beam_interpreter:new()),
    ?assertMatch({ok, _}, tflite_beam_interpreter:new()),
    Make = fun() ->
        [begin arm(interpreter_containers), catch tflite_beam_interpreter:new() end
         || _ <- lists:seq(1, 20000)]
    end,
    Growth = grew_by(Make),
    disarm(),
    ?assert(Growth < 1024 * 1024,
            lists:flatten(io_lib:format("~p bytes left behind by 20000 failures", [Growth]))).

builder_allocation_failure_is_reported_not_leaked(_Config) ->
    Model = tflite_beam_flatbuffer_model:build_from_file(tflite_beam_test_models:path("add.bin")),
    {ok, Resolver} = tflite_beam_ops_builtin_builtin_resolver:new(),
    arm(builder_containers),
    ?assertMatch({error, _}, tflite_beam_interpreter_builder:new(Model, Resolver)),
    ?assertMatch({ok, _}, tflite_beam_interpreter_builder:new(Model, Resolver)),
    Make = fun() ->
        [begin
            arm(builder_containers),
            catch tflite_beam_interpreter_builder:new(Model, Resolver)
         end || _ <- lists:seq(1, 20000)]
    end,
    Growth = grew_by(Make),
    disarm(),
    ?assert(Growth < 1024 * 1024,
            lists:flatten(io_lib:format("~p bytes left behind by 20000 failures", [Growth]))).

%% The reference on a delegate used to be taken before the list that records it
%% had room. A failure in between left a delegate nothing would ever release: the
%% builder had been told about it, our list had not, so no destructor was ever
%% going to give that reference back. Nothing crashes, which is why it needed
%% weighing rather than exercising. What is weighed is the resource struct, a few
%% hundred bytes each: the delegate's own memory comes from malloc and erlang:memory
%% cannot see it, so the count has to be high enough for the part it can see.
%%
%% Worth being exact about what this can and cannot catch, because it is weaker
%% than the case above. The stranded reference is the only consequence: the
%% builder still applies the delegate, and the leaked reference is what keeps it
%% alive, so nothing crashes and nothing else is observable from Erlang. That
%% means the case only reproduces while the fault point sits at whichever step
%% grows the list. Move the growth back after the retain, as the old code had it,
%% and the point has to move with it or this passes against the defect.
add_delegate_failure_strands_no_delegate(_Config) ->
    skip_without_xnnpack(fun() ->
        Fail = fun() ->
            [begin
                {Builder, _} = tflite_beam_test_models:builder("multi_add.bin"),
                {ok, Delegate} = tflite_beam_delegate:xnnpack(),
                arm(add_delegate_registry),
                true = is_oom(tflite_beam_interpreter_builder:add_delegate(Builder, Delegate))
             end || _ <- lists:seq(1, 20000)]
        end,
        Growth = grew_by(Fail),
        disarm(),
        %% and the builder is still one that works, so the fix did not buy this
        %% by refusing to add delegates at all
        {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
        {ok, Delegate} = tflite_beam_delegate:xnnpack(),
        ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),
        ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
        ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
        ?assert(Growth < 1024 * 1024,
                lists:flatten(io_lib:format("~p bytes stranded by 20000 failures", [Growth])))
    end).

%% Building into an interpreter used to install the new graph before the one
%% step in the handover that can fail, so a failure left the interpreter running
%% a graph built from the new model while its bookkeeping still named the old
%% one, and dropping the builder could then free what the graph was reading.
%% Doing that step before operator() runs means a failure changes nothing at all,
%% and that is what this checks rather than the ordering that produced it: an
%% interpreter built from a one-input model still has one input after a rebuild
%% from a four-input one has failed. It needs no delegate, because the handover
%% list is prepared on every build whether there are delegates in it or not.
delegate_transfer_failure_leaves_the_interpreter_untouched(_Config) ->
    {BuilderA, Interpreter} = plain_builder("add.bin"),
    ok = tflite_beam_interpreter_builder:build(BuilderA, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    ?assertEqual(1, length(inputs_of(Interpreter))),

    {BuilderB, _} = plain_builder("multi_add.bin"),
    arm(delegate_transfer),
    ?assert(is_oom(tflite_beam_interpreter_builder:build(BuilderB, Interpreter))),
    disarm(),

    %% still the model it was built from, and still usable
    ?assertEqual(1, length(inputs_of(Interpreter))),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    ?assert(is_integer(tflite_beam_interpreter:tensors_size(Interpreter))).

plain_builder(Name) ->
    tflite_beam_test_models:builder(Name, #{apply_default_delegates => false}).

inputs_of(Interpreter) ->
    {ok, Inputs} = tflite_beam_interpreter:inputs(Interpreter),
    Inputs.

skip_without_xnnpack(Body) ->
    case lists:member(xnnpack, tflite_beam_delegate:available()) of
        true -> Body();
        false -> {skip, "XNNPACK is not compiled into this build"}
    end.
