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
    ok = compiled_model_concurrency(Model),
    ok = compiled_model_lifetimes(Model),
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
    wait(spawn_many(12, fun() ->
        repeat(60, fun() ->
            _ = catch tflite_beam_interpreter:controlling_process(Interp, self()),
            _ = catch tflite_beam_interpreter:inputs(Interp),
            _ = catch tflite_beam_interpreter:execution_plan(Interp),
            _ = catch tflite_beam_interpreter:controlling_process(Interp),
            _ = catch tflite_beam_interpreter:controlling_process(Interp, undefined)
        end)
    end)),
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
            _ = ?A:run_with_metrics(CM, In),
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
        {error, _} = ?A:run(CM, tl(In)),
        {error, _} = ?A:run(CM, [binary:copy(<<0>>, 1) | tl(In)]),
        {error, _} = ?A:new(Env, Model, #{accelerators => [nope]}),
        {error, _} = ?A:new(Env, Model, #{signature => 99}),
        {error, _} = ?A:new(Env, Model, #{max_model_bytes => 1}),
        {error, _} = ?A:environment(<<"/tmp/a", 0, "b">>)
    end),
    [erlang:garbage_collect(P) || P <- erlang:processes()],
    ok.

spawn_many(N, Fun) ->
    [element(2, spawn_monitor(fun() -> Fun(), exit(done) end)) || _ <- lists:seq(1, N)].

wait(Refs) ->
    [receive {'DOWN', R, process, _, _} -> ok after 300000 -> ok end || R <- Refs],
    ok.

repeat(N, Fun) ->
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, N)).
