-module(benchmark_litert).
-export([main/0]).

-define(A, tflite_beam_litert_compiled_model).
-define(B, tflite_beam_litert_compiled_model_server).

main() ->
    [Path, CountString | InputPaths] = init:get_plain_arguments(),
    Count = list_to_integer(CountString),
    true = Count > 0,
    Dir = case os:getenv("LITERT_RUNTIME_LIBRARY_DIR") of false -> ""; Value -> Value end,
    Accelerator = case os:getenv("LITERT_BENCHMARK_ACCELERATOR") of
        "gpu" -> gpu;
        "npu" -> npu;
        _ -> cpu
    end,
    {ok, Env} = ?A:environment(Dir),
    Opts = #{accelerators => [Accelerator]},
    {CompileUs, {ok, Model}} = timer:tc(fun() -> ?A:new(Env, Path, Opts) end),
    {ok, {Sizes, _}} = ?A:io_sizes(Model),
    Inputs = inputs(InputPaths, Sizes),
    {ok, Expected} = ?A:run(Model, Inputs),
    {ok, Server} = ?B:start(Env, Path, Opts),
    Extra = [{<<"batch_size">>, 1}],
    Many = [{integer_to_binary(N), binary:copy(<<"x">>, 128)} || N <- lists:seq(1, 64)],
    Modes = [
        {run, fun() -> ?A:run(Model, Inputs) end},
        {metrics_default, fun() -> ?A:run_with_metrics(Model, Inputs) end},
        {metrics_timing, fun() -> ?A:run_with_metrics(Model, Inputs, #{host_timing => true}) end},
        {metrics_extra, fun() -> ?A:run_with_metrics(Model, Inputs, #{extra_metrics => Extra}) end},
        {metrics_many, fun() -> ?A:run_with_metrics(Model, Inputs,
            #{host_timing => true, extra_metrics => Many}) end},
        {server_run, fun() -> ?B:run(Server, Inputs) end},
        {server_metrics, fun() -> ?B:run_with_metrics(Server, Inputs, #{host_timing => true}) end}
    ],
    [{_, _} = sample(Fun, Expected) || {_, Fun} <- Modes, _ <- lists:seq(1, 10)],
    Samples = rounds(7, Modes, Count, Expected, #{}),
    ok = ?B:stop(Server),
    io:format("model=~ts accelerator=~p compile_us=~p non_cpu_fully_accelerated=~p~n",
        [Path, Accelerator, CompileUs, ?A:non_cpu_fully_accelerated(Model)]),
    io:format("mode,samples,mean_us,p50_us,p95_us,p99_us,mean_native_us~n"),
    [report(Name, maps:get(Name, Samples)) || {Name, _} <- Modes],
    halt().

inputs([], Sizes) ->
    [binary:copy(<<0>>, Size) || Size <- Sizes];
inputs(Paths, Sizes) ->
    true = length(Paths) =:= length(Sizes),
    [begin {ok, Binary} = file:read_file(Path), Size = byte_size(Binary), Binary end
     || {Path, Size} <- lists:zip(Paths, Sizes)].

sample(Fun, Expected) ->
    {Elapsed, Result} = timer:tc(Fun),
    Native = case Result of
        {ok, Expected} -> undefined;
        {ok, {Expected, Metrics}} -> proplists:get_value(<<"tflite_beam_run_us">>, Metrics)
    end,
    {Elapsed, Native}.

rounds(0, _Modes, _Count, _Expected, Acc) -> Acc;
rounds(N, Modes, Count, Expected, Acc) ->
    Next = lists:foldl(fun({Name, Fun}, Current) ->
        Values = [sample(Fun, Expected) || _ <- lists:seq(1, Count)],
        maps:update_with(Name, fun(Old) -> Values ++ Old end, Values, Current)
    end, Acc, Modes),
    [First | Rest] = Modes,
    rounds(N - 1, Rest ++ [First], Count, Expected, Next).

report(Name, Samples) ->
    Times = lists:sort([Elapsed || {Elapsed, _} <- Samples]),
    Native = [Time || {_, Time} <- Samples, is_integer(Time)],
    NativeMean = case Native of [] -> ""; _ -> io_lib:format("~.2f", [mean(Native)]) end,
    io:format("~p,~p,~.2f,~p,~p,~p,~s~n", [Name, length(Times), mean(Times),
        percentile(Times, 0.5), percentile(Times, 0.95), percentile(Times, 0.99), NativeMean]).

mean(Values) -> lists:sum(Values) / length(Values).

percentile(Sorted, Fraction) ->
    lists:nth(max(1, ceil(length(Sorted) * Fraction)), Sorted).
