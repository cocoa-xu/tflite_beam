-module(upgrade_drive).
-export([main/0]).
-include("../src/tflite_beam/tflite_beam_records.hrl").

main() ->
    register(upgrade_parent, self()),
    spawn(fun() -> receive stop -> ok after 55000 -> io:format("upgrade timeout~n"), halt(3) end end),
    load(0),
    {Pid, Monitor} = spawn_monitor(fun hold/0),
    receive {Pid, ready} -> ok after 10000 -> error(ready_timeout) end,
    check(Pid),
    load(1),
    check(Pid),
    io:format("separate image upgrade and old code purge passed~n"),
    [begin load(2 + (N rem 2)), check(Pid) end || N <- lists:seq(1, 20)],
    io:format("20 same-image upgrades and old code purges passed~n"),
    true = code:delete(tflite_beam_nif),
    true = code:soft_purge(tflite_beam_nif),
    Pid ! drop,
    receive
        {'DOWN', Monitor, process, Pid, normal} -> ok;
        {'DOWN', Monitor, process, Pid, Reason} -> error({holder_failed, Reason})
    after 10000 -> error(drop_timeout)
    end,
    erlang:garbage_collect(),
    timer:sleep(200),
    load(3),
    {After, AfterMonitor} = spawn_monitor(fun hold/0),
    receive {After, ready} -> ok after 10000 -> error(reload_timeout) end,
    check(After),
    After ! collect,
    receive {After, collected} -> ok after 10000 -> error(collect_timeout) end,
    After ! drop,
    receive {'DOWN', AfterMonitor, process, After, normal} -> ok
    after 10000 -> error(reload_drop_timeout)
    end,
    true = code:delete(tflite_beam_nif),
    true = code:soft_purge(tflite_beam_nif),
    erlang:garbage_collect(),
    timer:sleep(200),
    io:format("resources dropped after code deletion; final unload and reload passed~n"),
    io:format("upgrade drive complete~n"),
    halt(0).

load(Version) ->
    {ok, [[Dir]]} = init:get_argument(nif_upgrade_dir),
    Path = filename:join([Dir, "v" ++ integer_to_list(Version), "tflite_beam_nif.beam"]),
    {ok, Binary} = file:read_file(Path),
    {module, tflite_beam_nif} = code:load_binary(tflite_beam_nif, Path, Binary),
    true = code:soft_purge(tflite_beam_nif),
    Version = tflite_beam_nif:upgrade_generation().

check(Pid) ->
    Pid ! {check, self()},
    receive {Pid, checked} -> ok after 10000 -> error(check_timeout) end.

hold() ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    {ok, [Index | _]} = tflite_beam_interpreter:inputs(Interpreter),
    Tensors = [tflite_beam_interpreter:tensor(Interpreter, Index) || _ <- lists:seq(1, 100)],
    Runners = [begin
        {ok, Runner} = tflite_beam_interpreter:get_signature_runner(Interpreter, nil),
        Runner
    end || _ <- lists:seq(1, 100)],
    {ok, Env} = tflite_beam_litert_compiled_model:environment(),
    {ok, Model} = tflite_beam_litert_compiled_model:new(Env, tflite_beam_test_models:path("multi_add.bin")),
    {ok, {Sizes, _}} = tflite_beam_litert_compiled_model:io_sizes(Model),
    Inputs = [binary:copy(<<0>>, N) || N <- Sizes],
    {ok, Expected} = tflite_beam_litert_compiled_model:run(Model, Inputs),
    Parent = whereis(upgrade_parent),
    Parent ! {self(), ready},
    hold(Interpreter, Tensors, Runners, Model, Inputs, Expected).

hold(Interpreter, Tensors, Runners, Model, Inputs, Expected) ->
    receive
        {check, Parent} ->
            {ok, {100, 100}} = tflite_beam_nif:nif_interpreter_registry_sizes(Interpreter),
            [begin
                #tflite_beam_tensor{ref = Ref} = Tensor,
                {f, 32} = tflite_beam_tensor:type(Ref),
                true = is_binary(tflite_beam_tensor:to_binary(Tensor))
            end || Tensor <- Tensors],
            [{ok, _} = tflite_beam_signature_runner:input_size(Runner) || Runner <- Runners],
            {ok, Expected} = tflite_beam_litert_compiled_model:run(Model, Inputs),
            Parent ! {self(), checked},
            hold(Interpreter, Tensors, Runners, Model, Inputs, Expected);
        collect ->
            ok = tflite_beam_nif:nif_arm_fault(litert_reaper_submit),
            erlang:garbage_collect(),
            whereis(upgrade_parent) ! {self(), collected},
            hold(Interpreter, Tensors, Runners, Model, Inputs, Expected);
        drop -> ok
    end.
