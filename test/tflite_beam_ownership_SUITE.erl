%% @doc Who is allowed to use an interpreter.
%%
%% Two mechanisms, and they answer different questions. The in-use guard is
%% always on and refuses calls that genuinely overlap in time. A controlling
%% process is opt-in and refuses every other process, overlapping or not --
%% which is what closes the window the first one cannot see, where two processes
%% take turns badly rather than colliding.
-module(tflite_beam_ownership_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("../src/tflite_beam/tflite_beam_records.hrl").

-export([all/0]).
-export([
    a_handle_answers_to_the_controlling_process/1,
    shared_by_default/1,
    controlled_use_from_the_controlling_process/1,
    refused_from_another_process/1,
    taking_it_twice_is_idempotent/1,
    only_the_controller_may_transfer/1,
    transfer_to_another_process/1,
    undefined_gives_it_back/1,
    a_dead_controller_releases_it/1,
    signature_runner_inherits_it/1,
    concurrent_use_is_refused/1,
    server_predicts/1,
    server_is_atomic_under_concurrency/1,
    server_keeps_its_interpreter_to_itself/1,
    server_with_runs_inside_the_owner/1
]).

-define(FILLED(V), binary:copy(<<V:32/float-native>>, 1 * 8 * 8 * 3)).
-define(INPUTS, [?FILLED(1.0), ?FILLED(2.0), ?FILLED(3.0), ?FILLED(4.0)]).
-define(ROUNDS, 3000).

all() ->
    [
        a_handle_answers_to_the_controlling_process,
        shared_by_default,
        controlled_use_from_the_controlling_process,
        refused_from_another_process,
        taking_it_twice_is_idempotent,
        only_the_controller_may_transfer,
        transfer_to_another_process,
        undefined_gives_it_back,
        a_dead_controller_releases_it,
        signature_runner_inherits_it,
        concurrent_use_is_refused,
        server_predicts,
        server_is_atomic_under_concurrency,
        server_keeps_its_interpreter_to_itself,
        server_with_runs_inside_the_owner
    ].

%% The regression guard. An interpreter belongs to nobody until someone says
%% otherwise, and two processes taking turns both work -- exactly as they always
%% have. A change that refuses everything must fail here.
shared_by_default(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ?assertEqual(undefined, tflite_beam_interpreter:controlling_process(Interpreter)),
    ?assertEqual([?FILLED(6.0), ?FILLED(9.0)], predict(Interpreter)),
    ?assertEqual([?FILLED(6.0), ?FILLED(9.0)], in_another_process(fun() -> predict(Interpreter) end)).

controlled_use_from_the_controlling_process(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ok = tflite_beam_interpreter:controlling_process(Interpreter, self()),
    ?assertEqual({ok, self()}, tflite_beam_interpreter:controlling_process(Interpreter)),
    ?assertEqual([?FILLED(6.0), ?FILLED(9.0)], predict(Interpreter)),
    ?assertEqual(7, tflite_beam_interpreter:tensors_size(Interpreter)).

%% The whole point: the second process is told, and the VM stays up.
refused_from_another_process(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ok = tflite_beam_interpreter:controlling_process(Interpreter, self()),
    Refused = in_another_process(fun() ->
        [tflite_beam_interpreter:invoke(Interpreter),
         tflite_beam_interpreter:allocate_tensors(Interpreter),
         tflite_beam_interpreter:tensor(Interpreter, 0),
         tflite_beam_interpreter:execution_plan(Interpreter)]
    end),
    [?assertMatch({error, _}, R) || R <- Refused],
    %% and the controlling process is unaffected by any of that
    ?assertEqual([?FILLED(6.0), ?FILLED(9.0)], predict(Interpreter)).

taking_it_twice_is_idempotent(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ok = tflite_beam_interpreter:controlling_process(Interpreter, self()),
    ?assertEqual(ok, tflite_beam_interpreter:controlling_process(Interpreter, self())),
    ?assertEqual({ok, self()}, tflite_beam_interpreter:controlling_process(Interpreter)).

%% gen_tcp's rule: only the process that holds it may hand it on.
only_the_controller_may_transfer(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ok = tflite_beam_interpreter:controlling_process(Interpreter, self()),
    Elsewhere = in_another_process(fun() ->
        tflite_beam_interpreter:controlling_process(Interpreter, self())
    end),
    ?assertMatch({error, _}, Elsewhere),
    ?assertEqual({ok, self()}, tflite_beam_interpreter:controlling_process(Interpreter)).

transfer_to_another_process(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ok = tflite_beam_interpreter:controlling_process(Interpreter, self()),
    Parent = self(),
    %% the new owner has to outlive the assertions below: a controlling process
    %% that exits releases the interpreter, which would hand it straight back
    {Pid, Ref} = spawn_monitor(fun() ->
        receive yours -> Parent ! {self(), predict(Interpreter)} end,
        receive finished -> ok end
    end),
    ok = tflite_beam_interpreter:controlling_process(Interpreter, Pid),
    ?assertEqual({ok, Pid}, tflite_beam_interpreter:controlling_process(Interpreter)),
    Pid ! yours,
    receive {Pid, Result} -> ?assertEqual([?FILLED(6.0), ?FILLED(9.0)], Result)
    after 30000 -> ct:fail(timeout) end,
    %% having given it away, we are now the other process
    ?assertMatch({error, _}, tflite_beam_interpreter:invoke(Interpreter)),
    Pid ! finished,
    receive {'DOWN', Ref, process, Pid, _} -> ok after 30000 -> ct:fail(timeout) end,
    %% and once it is gone the interpreter is free again
    ?assertEqual(ok, tflite_beam_interpreter:allocate_tensors(Interpreter)).

undefined_gives_it_back(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    ok = tflite_beam_interpreter:controlling_process(Interpreter, self()),
    ?assertMatch({error, _}, in_another_process(fun() -> tflite_beam_interpreter:invoke(Interpreter) end)),
    ok = tflite_beam_interpreter:controlling_process(Interpreter, undefined),
    ?assertEqual(undefined, tflite_beam_interpreter:controlling_process(Interpreter)),
    ?assertEqual(ok, in_another_process(fun() -> tflite_beam_interpreter:invoke(Interpreter) end)).

%% An interpreter has no equivalent of a socket being closed, so a controlling
%% process that dies leaves it to whoever wants it rather than stranding it.
a_dead_controller_releases_it(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        ok = tflite_beam_interpreter:controlling_process(Interpreter, self()),
        Parent ! taken
    end),
    receive taken -> ok after 30000 -> ct:fail(timeout) end,
    receive {'DOWN', Ref, process, Pid, _} -> ok after 30000 -> ct:fail(timeout) end,
    ?assertEqual(ok, tflite_beam_interpreter:allocate_tensors(Interpreter)),
    ?assertEqual(ok, tflite_beam_interpreter:controlling_process(Interpreter, self())).

%% A runner is a view onto its interpreter's subgraph, so it answers to whoever
%% controls that interpreter.
signature_runner_inherits_it(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    {ok, Runner} = tflite_beam_interpreter:get_signature_runner(Interpreter, <<"serving_default">>),
    ok = tflite_beam_interpreter:controlling_process(Interpreter, self()),
    ?assertMatch({ok, _}, tflite_beam_signature_runner:input_names(Runner)),
    ?assertMatch({error, _},
                 in_another_process(fun() -> tflite_beam_signature_runner:input_names(Runner) end)).

%% The always-on half, which needs no opt-in. Two processes hammering one
%% interpreter overlap often enough to be caught: measured at roughly 3% of
%% calls on this fixture, so a few thousand rounds puts the count near a hundred
%% rather than near zero.
concurrent_use_is_refused(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("multi_add.bin"),
    Parent = self(),
    Hammer = fun(Value) ->
        fun() ->
            Refused = lists:foldl(fun(_, Acc) ->
                case tflite_beam_interpreter:input_tensor(Interpreter, 0, ?FILLED(Value)) of
                    {error, _} -> Acc + 1;
                    ok ->
                        case tflite_beam_interpreter:invoke(Interpreter) of
                            {error, _} -> Acc + 1;
                            ok -> Acc
                        end
                end
            end, 0, lists:seq(1, ?ROUNDS)),
            Parent ! {self(), Refused}
        end
    end,
    First = spawn(Hammer(1.0)),
    Second = spawn(Hammer(2.0)),
    A = receive {First, X} -> X after 120000 -> ct:fail(timeout) end,
    B = receive {Second, Y} -> Y after 120000 -> ct:fail(timeout) end,
    ct:comment("refused ~p of ~p calls", [A + B, 2 * ?ROUNDS]),
    ?assert(A + B > 0).

predict(Interpreter) ->
    tflite_beam_interpreter:predict(Interpreter, ?INPUTS).

in_another_process(Body) ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() -> Parent ! {self(), Body()} end),
    Result = receive {Pid, R} -> R after 60000 -> ct:fail(timeout) end,
    receive {'DOWN', Ref, process, Pid, _} -> ok after 30000 -> ct:fail(timeout) end,
    Result.

server_predicts(_Config) ->
    {ok, Server} = tflite_beam_interpreter_server:start(tflite_beam_test_models:path("multi_add.bin")),
    ?assertEqual([?FILLED(6.0), ?FILLED(9.0)], tflite_beam_interpreter_server:predict(Server, ?INPUTS)),
    ok = tflite_beam_interpreter_server:stop(Server).

%% The reason this module exists. Feeding, running and reading back is one step
%% that nothing interleaves with, so two processes both get their own answer --
%% where the same thing done directly gets them each other's.
server_is_atomic_under_concurrency(_Config) ->
    {ok, Server} = tflite_beam_interpreter_server:start(tflite_beam_test_models:path("multi_add.bin")),
    Parent = self(),
    Rounds = 300,
    Hammer = fun(Inputs, Expected) ->
        fun() ->
            Wrong = lists:foldl(fun(_, Acc) ->
                case tflite_beam_interpreter_server:predict(Server, Inputs) of
                    Expected -> Acc;
                    _ -> Acc + 1
                end
            end, 0, lists:seq(1, Rounds)),
            Parent ! {self(), Wrong}
        end
    end,
    Ones = [?FILLED(1.0), ?FILLED(1.0), ?FILLED(1.0), ?FILLED(1.0)],
    First = spawn(Hammer(?INPUTS, [?FILLED(6.0), ?FILLED(9.0)])),
    Second = spawn(Hammer(Ones, [?FILLED(3.0), ?FILLED(3.0)])),
    A = receive {First, X} -> X after 120000 -> ct:fail(timeout) end,
    B = receive {Second, Y} -> Y after 120000 -> ct:fail(timeout) end,
    ?assertEqual(0, A + B),
    ok = tflite_beam_interpreter_server:stop(Server).

%% Nothing outside can reach the interpreter, even holding a reference to it,
%% because the server took control of it on the way up.
server_keeps_its_interpreter_to_itself(_Config) ->
    {ok, Server} = tflite_beam_interpreter_server:start(tflite_beam_test_models:path("multi_add.bin")),
    Interpreter = tflite_beam_interpreter_server:with(Server, fun(I) -> I end),
    ?assertMatch({error, _}, tflite_beam_interpreter:invoke(Interpreter)),
    ?assertMatch({error, _}, tflite_beam_interpreter:controlling_process(Interpreter, self())),
    ?assertEqual([?FILLED(6.0), ?FILLED(9.0)], tflite_beam_interpreter_server:predict(Server, ?INPUTS)),
    ok = tflite_beam_interpreter_server:stop(Server).

%% with/2 is the escape hatch for sequences predict/2 does not cover, and it has
%% to run where the interpreter is reachable.
server_with_runs_inside_the_owner(_Config) ->
    {ok, Server} = tflite_beam_interpreter_server:start(tflite_beam_test_models:path("multi_add.bin")),
    Sizes = tflite_beam_interpreter_server:with(Server, fun(I) ->
        {tflite_beam_interpreter:tensors_size(I), tflite_beam_interpreter:inputs(I)}
    end),
    ?assertEqual({7, {ok, [0, 1, 2, 3]}}, Sizes),
    ok = tflite_beam_interpreter_server:stop(Server).

%% The guard was on the door and not on the window. interpreter:tensor/2 refused
%% a process that did not control the interpreter, and a handle that process
%% already held read and wrote through it regardless, with the owner seeing the
%% foreign write. tflite_beam_interpreter_server hands out exactly such a handle
%% from with/2, so its isolation was undone by its own escape hatch.
a_handle_answers_to_the_controlling_process(_Config) ->
    Interpreter = tflite_beam_test_models:interpreter("add.bin"),
    ok = tflite_beam_interpreter:controlling_process(Interpreter, self()),
    {ok, [Index | _]} = tflite_beam_interpreter:inputs(Interpreter),
    Handle = tflite_beam_interpreter:tensor(Interpreter, Index),
    Mine = binary:copy(<<0>>, 768),
    ok = tflite_beam_tensor:set_data(Handle, Mine),

    Parent = self(),
    spawn(fun() ->
        Parent ! {read, tflite_beam_tensor:to_binary(Handle)},
        Parent ! {write, tflite_beam_tensor:set_data(Handle, binary:copy(<<3>>, 768))}
    end),
    Read = receive {read, R} -> R after 30000 -> timeout end,
    Write = receive {write, W} -> W after 30000 -> timeout end,

    ?assertMatch({error, _}, Read),
    ?assertMatch({error, _}, Write),

    %% and the owner still sees what the owner wrote
    ?assertEqual(Mine, tflite_beam_tensor:to_binary(Handle)).
