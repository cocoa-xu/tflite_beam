%% @doc
%% What scripts/run_sanitizer.sh drives.
%%
%% Not a Common Test suite, and deliberately so: what it exercises is
%% concurrency and lifetime, which the suite cannot assert on because a data
%% race has no return value. Three separate races in the interpreter's ownership
%% state passed 155 test cases without a murmur and were found here.
%%
%% Every section is here because something was actually found in it. Adding a
%% section is cheap; leaving one out is how a defect gets a home.
-module(sanitizer_drive).
-export([main/0]).

-define(A, tflite_beam_litert_compiled_model).
-define(B, tflite_beam_litert_compiled_model_server).

-include("../src/tflite_beam/tflite_beam_records.hrl").

main() ->
    Root = root_dir(),
    Model = filename:join([Root, "test", "models", "multi_add.bin"]),
    ok = interpreter_ownership(Model),
    ok = interpreter_rebuild_concurrency(Model),
    ok = interpreter_registry_concurrency(Model),
    ok = compiled_model_concurrency(Model),
    ok = compiled_model_lifetimes(Model),
    ok = backend_options_lifetimes(Model),
    ok = graph_rewrite_lifetimes(Root),
    io:format("sanitizer drive complete~n"),
    halt(0).

root_dir() ->
    case init:get_argument(root_dir) of
        {ok, [[Dir]]} -> Dir;
        _ -> "."
    end.

%% Claim, use and hand back, over and over from many processes. Claiming once
%% and stopping leaves the write path almost unexercised, which is what a drive
%% that finds nothing looks like: the first process wins and the rest are
%% refused without anything being written again.
interpreter_ownership(Model) ->
    {ok, Resolver} = tflite_beam_ops_builtin_builtin_resolver:new(#{}),
    #tflite_beam_flatbuffer_model{ref = Ref} =
        tflite_beam_flatbuffer_model:build_from_file(Model),
    {ok, Builder} = tflite_beam_interpreter_builder:new(Ref, Resolver),
    {ok, Interp} = tflite_beam_interpreter:new(),
    ok = tflite_beam_interpreter_builder:build(Builder, Interp),
    ok = tflite_beam_interpreter:allocate_tensors(Interp),
    {ok, [Index | _]} = tflite_beam_interpreter:inputs(Interp),
    #tflite_beam_tensor{ref = Tensor} = tflite_beam_interpreter:tensor(Interp, Index),
    {ok, Runner} = tflite_beam_interpreter:get_signature_runner(Interp, nil),
    wait(spawn_many(12, fun() ->
        repeat(60, fun() ->
            _ = catch tflite_beam_interpreter:controlling_process(Interp, self()),
            _ = catch tflite_beam_interpreter:inputs(Interp),
            _ = catch tflite_beam_interpreter:execution_plan(Interp),
            _ = catch tflite_beam_interpreter:controlling_process(Interp),
            _ = tflite_beam_tensor:type(Tensor),
            _ = tflite_beam_signature_runner:input_size(Runner),
            _ = catch tflite_beam_interpreter:controlling_process(Interp, undefined)
        end)
    end)),
    ok.

interpreter_rebuild_concurrency(Model) ->
    {ok, Resolver} = tflite_beam_ops_builtin_builtin_resolver:new(#{}),
    #tflite_beam_flatbuffer_model{ref = Ref} =
        tflite_beam_flatbuffer_model:build_from_file(Model),
    {ok, Builder} = tflite_beam_interpreter_builder:new(Ref, Resolver),
    {ok, Interpreter} = tflite_beam_interpreter:new(),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    Readers = spawn_many(8, fun() ->
        repeat(1000, fun() ->
            _ = tflite_beam_interpreter:inputs(Interpreter),
            _ = tflite_beam_interpreter:controlling_process(Interpreter),
            _ = tflite_beam_interpreter:cancel(Interpreter)
        end)
    end),
    repeat(100, fun() ->
        _ = tflite_beam_interpreter_builder:build(Builder, Interpreter)
    end),
    wait(Readers),
    {ok, [_ | _]} = tflite_beam_interpreter:inputs(Interpreter),
    ok.

%% Every compiled model entry point at once, including the ones that read while
%% another thread writes: the profile, the ownership state, the metrics.
compiled_model_concurrency(Model) ->
    {ok, Env} = ?A:environment(),
    {ok, CM} = ?A:new(Env, Model, #{accelerators => [cpu], profile => true}),
    {ok, {Ins, _}} = ?A:io_sizes(CM),
    In = [binary:copy(<<0>>, N) || N <- Ins],
    wait(spawn_many(10, fun() ->
        repeat(12, fun() ->
            _ = ?A:run(CM, In),
            _ = ?A:profile(CM, 5),
            _ = ?A:summarise_profile(CM),
            _ = ?A:controlling_process(CM),
            _ = ?A:io_sizes(CM),
            _ = ?A:fully_accelerated(CM),
            _ = ?A:non_cpu_fully_accelerated(CM),
            _ = ?A:run_with_metrics(CM, In),
            _ = ?A:run_with_metrics(CM, In, #{host_timing => true,
                extra_metrics => [{<<"tag">>, binary:copy(<<"x">>, 128)}]}),
            _ = ?A:reset_profile(CM)
        end)
    end)),
    {ok, Server} = ?B:start(Env, Model, #{accelerators => [cpu]}),
    wait(spawn_many(6, fun() ->
        repeat(10, fun() ->
            _ = ?B:run(Server, In),
            _ = ?B:summarise_profile(Server)
        end)
    end)),
    ?B:stop(Server),
    ok.

interpreter_registry_concurrency(Model) ->
    {ok, Resolver} = tflite_beam_ops_builtin_builtin_resolver:new(#{}),
    #tflite_beam_flatbuffer_model{ref = Ref} = tflite_beam_flatbuffer_model:build_from_file(Model),
    {ok, Builder} = tflite_beam_interpreter_builder:new(Ref, Resolver),
    {ok, Interpreter} = tflite_beam_interpreter:new(),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    {ok, [Index | _]} = tflite_beam_interpreter:inputs(Interpreter),
    Workers = [spawn_monitor(fun registry_collect/0) || _ <- lists:seq(1, 10)],
    repeat(20, fun() ->
        [Pid ! {batch, registry_batch(Interpreter, Index)} || {Pid, _} <- Workers]
    end),
    [Pid ! done || {Pid, _} <- Workers],
    wait([Monitor || {_, Monitor} <- Workers]),
    erlang:garbage_collect(),
    registry_empty(Interpreter, 200).

registry_batch(Interpreter, Index) ->
    Tensors = [begin
        #tflite_beam_tensor{} = Tensor = tflite_beam_interpreter:tensor(Interpreter, Index),
        Tensor
    end || _ <- lists:seq(1, 30)],
    Runners = [begin
        {ok, Runner} = tflite_beam_interpreter:get_signature_runner(Interpreter, nil),
        Runner
    end || _ <- lists:seq(1, 30)],
    {Tensors, Runners}.

registry_collect() ->
    case registry_receive() of
        batch -> erlang:garbage_collect(), registry_collect();
        done -> exit(done)
    end.

registry_receive() ->
    receive
        {batch, {Tensors, Runners}} -> 60 = length(Tensors) + length(Runners), batch;
        done -> done
    end.

registry_empty(Interpreter, Remaining) ->
    case tflite_beam_nif:nif_interpreter_registry_sizes(Interpreter) of
        {ok, {0, 0}} -> ok;
        _ when Remaining > 0 -> timer:sleep(10), registry_empty(Interpreter, Remaining - 1);
        Counts -> error({registry_not_empty, Counts})
    end.

%% Build and drop many models, through the error paths as well as the good one:
%% a double free lives in the path that failed, not the one that worked.
compiled_model_lifetimes(Model) ->
    {ok, Env} = ?A:environment(),
    repeat(40, fun() ->
        {ok, CM} = ?A:new(Env, Model, #{accelerators => [cpu]}),
        {ok, {Ins, _}} = ?A:io_sizes(CM),
        In = [binary:copy(<<0>>, N) || N <- Ins],
        {ok, _} = ?A:run(CM, In),
        {ok, _} = ?A:run_with_metrics(CM, In),
        {ok, false} = ?A:non_cpu_fully_accelerated(CM),
        Extra = [{integer_to_binary(N), binary:copy(<<"x">>, N + 64)}
                 || N <- lists:seq(1, 64)],
        {ok, {_, [{<<"tflite_beam_run_us">>, Time} | Extra]}} =
            ?A:run_with_metrics(CM, In, #{host_timing => true, extra_metrics => Extra}),
        true = is_integer(Time) andalso Time >= 0,
        {error, _} = ?A:run_with_metrics(CM, In, #{extra_metrics => [{<<"tag">>, <<0>>}]}),
        {error, _} = ?A:run(CM, tl(In)),
        {error, _} = ?A:run(CM, [binary:copy(<<0>>, 1) | tl(In)]),
        {error, _} = ?A:new(Env, Model, #{accelerators => [nope]}),
        {error, _} = ?A:new(Env, Model, #{signature => 99}),
        {error, _} = ?A:new(Env, Model, #{max_model_bytes => 1}),
        {error, _} = ?A:environment(<<"/tmp/a", 0, "b">>)
    end),
    [erlang:garbage_collect(P) || P <- erlang:processes()],
    ok.

backend_options_lifetimes(Model) ->
    {ok, Env} = ?A:environment(),
    Options = #{cpu => #{num_threads => 2}, gpu => #{use_ir_model => true},
        qualcomm => #{qnn_lib_dir => binary:copy(<<"path">>, 64)},
        google_tensor => #{input_coherency => [{<<"serving">>, binary:copy(<<"input">>, 32), true}]},
        mediatek => #{option_bundle => binary:copy(<<"bundle">>, 32)},
        arm => #{enable_just_in_time => true}, intel_openvino => #{enable_weight_sharing => true}},
    repeat(40, fun() ->
        {ok, _} = ?A:backend_options(Options),
        {ok, CM} = ?A:new(Env, Model, #{backend_options => Options}),
        {ok, {Sizes, _}} = ?A:io_sizes(CM),
        {ok, _} = ?A:run(CM, [binary:copy(<<0>>, N) || N <- Sizes]),
        {error, _} = ?A:backend_options(Options#{qualcomm := #{qnn_lib_dir => <<0>>}})
    end),
    ok.

graph_rewrite_lifetimes(Root) ->
    {ok, Env} = ?A:environment(),
    Path = filename:join([Root, "test", "models", "graph_options.tflite"]),
    {ok, Source} = file:read_file(Path),
    {ok, [_, _, #{operators := [#{options := Options}]}]} =
        tflite_beam_litert_model:inspect(Env, {buffer, Source}),
    repeat(80, fun() ->
        Changed = Options#{name := binary:copy(<<"name">>, 64)},
        Edit = #{subgraph => 2, operator => 0, options => Changed},
        {ok, Binary} = tflite_beam_litert_model:rewrite(Env, {buffer, Source}, [Edit]),
        {ok, [_, _, #{operators := [#{options := Changed}]}]} =
            tflite_beam_litert_model:inspect(Env, {buffer, Binary}),
        {error, _} = tflite_beam_litert_model:rewrite(Env, {buffer, Source},
            [Edit#{options := Changed#{attributes := <<1, 2, 3>>}}])
    end),
    ok.

spawn_many(N, Fun) ->
    [element(2, spawn_monitor(fun() -> Fun(), exit(done) end)) || _ <- lists:seq(1, N)].

wait(Refs) ->
    [receive
        {'DOWN', R, process, _, done} -> ok;
        {'DOWN', R, process, _, Reason} -> error({worker_failed, Reason})
     after 300000 -> error(worker_timeout)
     end || R <- Refs],
    ok.

repeat(N, Fun) ->
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, N)).
