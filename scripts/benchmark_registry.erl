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
    halt().

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
