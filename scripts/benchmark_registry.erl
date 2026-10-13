-module(benchmark_registry).
-export([main/0]).

main() ->
    Interpreter = tflite_beam_test_models:interpreter("add.bin"),
    {ok, [Index | _]} = tflite_beam_interpreter:inputs(Interpreter),
    io:format("count,create_us,collect_us,total_us~n"),
    [begin
        Build = fetch(Interpreter, Index, Count),
        {Collect, ok} = timer:tc(fun() ->
            erlang:garbage_collect(),
            await_empty(Interpreter, erlang:monotonic_time(millisecond) + 5000)
        end),
        io:format("~p,~p,~p,~p~n", [Count, Build, Collect, Build + Collect])
    end || Count <- [1000, 5000, 20000], _ <- lists:seq(1, 7)],
    io:format("retired_count,live_count,retired_collect_us~n"),
    [begin
        Time = collect_retired_generation(Interpreter, Index, Count),
        io:format("~p,~p,~p~n", [Count, Count, Time]),
        erlang:garbage_collect(),
        await_empty(Interpreter, erlang:monotonic_time(millisecond) + 5000)
    end || Count <- [1000, 5000, 20000], _ <- lists:seq(1, 7)],
    halt().

collect_retired_generation(Interpreter, Index, Count) ->
    Parent = self(),
    {Pid, Monitor} = spawn_monitor(fun() ->
        Tensors = fetch_handles(Interpreter, Index, Count),
        Parent ! {self(), ready},
        receive release -> Count = length(Tensors) end
    end),
    receive {Pid, ready} -> ok after 5000 -> error(worker_timeout) end,
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    Live = fetch_handles(Interpreter, Index, Count),
    {Time, ok} = timer:tc(fun() ->
        Pid ! release,
        receive
            {'DOWN', Monitor, process, Pid, normal} -> ok;
            {'DOWN', Monitor, process, Pid, Reason} -> error({worker_failed, Reason})
        after 5000 -> error(worker_timeout)
        end
    end),
    {ok, {Count, 0}} = tflite_beam_nif:nif_interpreter_registry_sizes(Interpreter),
    Count = length(Live),
    Time.

fetch_handles(Interpreter, Index, Count) ->
    [tflite_beam_interpreter:tensor(Interpreter, Index) || _ <- lists:seq(1, Count)].

fetch(Interpreter, Index, Count) ->
    {Time, Tensors} = timer:tc(fun() ->
        [tflite_beam_interpreter:tensor(Interpreter, Index) || _ <- lists:seq(1, Count)]
    end),
    {ok, {Count, 0}} = tflite_beam_nif:nif_interpreter_registry_sizes(Interpreter),
    Count = length(Tensors),
    Time.

await_empty(Interpreter, Deadline) ->
    case tflite_beam_nif:nif_interpreter_registry_sizes(Interpreter) of
        {ok, {0, 0}} -> ok;
        Counts ->
            case erlang:monotonic_time(millisecond) < Deadline of
                true -> erlang:yield(), await_empty(Interpreter, Deadline);
                false -> error({registry_not_empty, Counts})
            end
    end.
