%% @doc LiteRT's compiled model, the profiler that only it has, and the process
%% that keeps two callers from sharing its buffers. Skipped whole when the
%% library was built without the LiteRT API.
-module(tflite_beam_litert_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    environment_is_created/1,
    compiled_model_runs/1,
    compiled_model_refuses_a_wrong_sized_input/1,
    compiled_model_refuses_the_wrong_number_of_inputs/1,
    platform_support_is_reported/1,
    hostile_arguments_are_refused_not_survived/1,
    signatures_are_listed/1,
    a_signature_can_be_named/1,
    each_signature_runs_its_own_function/1,
    an_unknown_signature_is_refused/1,
    metrics_are_empty_rather_than_an_error/1,
    profile_is_empty_unless_asked_for/1,
    profile_names_the_operators/1,
    reset_profile_needs_profiling_on/1,
    server_serialises_what_sharing_gets_wrong/1,
    an_unclaimed_model_is_open_to_every_process/1,
    a_claimed_model_refuses_other_processes/1,
    a_claim_dies_with_its_process/1,
    the_server_claims_what_with_hands_out/1,
    a_bad_detail_level_does_not_take_the_server_down/1
]).

-define(A, tflite_beam_litert_compiled_model).
-define(B, tflite_beam_litert_compiled_model_server).
-define(MODEL, "multi_add.bin").

all() ->
    [
        environment_is_created,
        compiled_model_runs,
        compiled_model_refuses_a_wrong_sized_input,
        compiled_model_refuses_the_wrong_number_of_inputs,
        platform_support_is_reported,
        hostile_arguments_are_refused_not_survived,
        signatures_are_listed,
        a_signature_can_be_named,
        each_signature_runs_its_own_function,
        an_unknown_signature_is_refused,
        metrics_are_empty_rather_than_an_error,
        profile_is_empty_unless_asked_for,
        profile_names_the_operators,
        reset_profile_needs_profiling_on,
        server_serialises_what_sharing_gets_wrong,
        an_unclaimed_model_is_open_to_every_process,
        a_claimed_model_refuses_other_processes,
        a_claim_dies_with_its_process,
        the_server_claims_what_with_hands_out,
        a_bad_detail_level_does_not_take_the_server_down
    ].

%% The Erlang stubs exist whatever the library was built with; what is missing
%% without the LiteRT API is the NIF behind them, and calling one then raises
%% rather than returning an error tuple.
init_per_suite(Config) ->
    case catch ?A:platform_support() of
        {error, _} ->
            %% the NIFs are registered but report the feature is absent, which is
            %% what a build without TFLITE_BEAM_ENABLE_LITERT_API now does
            {skip, "built without TFLITE_BEAM_ENABLE_LITERT_API"};
        Support when is_map(Support) ->
            %% The feature is compiled in, so from here an error is a failure and
            %% not a reason to skip: a suite that skips itself when the thing it
            %% tests is broken is worse than no suite.
            {ok, Env} = ?A:environment(),
            [{env, Env} | Config];
        _ ->
            {skip, "built without TFLITE_BEAM_ENABLE_LITERT_API"}
    end.

end_per_suite(_Config) ->
    ok.

model(Config, Opts) ->
    Env = proplists:get_value(env, Config),
    {ok, Model} = ?A:new(Env, tflite_beam_test_models:path(?MODEL), Opts),
    Model.

filled(Value, Bytes) ->
    binary:copy(<<Value:32/float-native>>, Bytes div 4).

environment_is_created(Config) ->
    ?assert(is_reference(proplists:get_value(env, Config))).

compiled_model_runs(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    {ok, {Ins, Outs}} = ?A:io_sizes(Model),
    ?assertEqual(4, length(Ins)),
    ?assertEqual(2, length(Outs)),
    Inputs = [filled(V, N) || {V, N} <- lists:zip([1.0, 2.0, 3.0, 4.0], Ins)],
    {ok, Got} = ?A:run(Model, Inputs),
    %% x = a + (b + c), y = d + (b + c)
    [X, Y] = Got,
    ?assertEqual(filled(6.0, hd(Outs)), X),
    ?assertEqual(filled(9.0, hd(tl(Outs))), Y),
    ?assertMatch({ok, _}, ?A:fully_accelerated(Model)).

compiled_model_refuses_a_wrong_sized_input(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    {ok, {Ins, _}} = ?A:io_sizes(Model),
    Right = [filled(1.0, N) || N <- Ins],
    %% the same call with the right sizes has to work, or this proves nothing
    ?assertMatch({ok, _}, ?A:run(Model, Right)),
    Short = [binary:part(hd(Right), 0, hd(Ins) - 4) | tl(Right)],
    ?assertMatch({error, _}, ?A:run(Model, Short)).

compiled_model_refuses_the_wrong_number_of_inputs(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    {ok, {Ins, _}} = ?A:io_sizes(Model),
    Right = [filled(1.0, N) || N <- Ins],
    ?assertMatch({ok, _}, ?A:run(Model, Right)),
    ?assertMatch({error, _}, ?A:run(Model, tl(Right))),
    ?assertMatch({error, _}, ?A:run(Model, Right ++ [hd(Right)])).

%% Compile-time capabilities, so this asserts the shape and the one answer the
%% platform fixes rather than a value that varies by machine.
platform_support_is_reported(_Config) ->
    Support = ?A:platform_support(),
    ?assert(is_map(Support)),
    Expected = [opencl, opengl, metal, ahwb, ion, dmabuf, fastrpc, sync_fence],
    ?assertEqual(lists:sort(Expected), lists:sort(maps:keys(Support))),
    ?assert(lists:all(fun is_boolean/1, maps:values(Support))),
    %% LiteRT fixes these two by platform, whatever is installed
    case os:type() of
        {unix, darwin} ->
            ?assertEqual(true, maps:get(metal, Support)),
            ?assertEqual(false, maps:get(opencl, Support));
        _ ->
            ?assertEqual(false, maps:get(metal, Support))
    end.

%% Degenerate and hostile arguments from Erlang. Every one of these has to be
%% refused with something a caller can act on, and the model has to still work
%% afterwards: a refusal that leaves the resource unusable is only half a
%% refusal. A zero byte matters more than it looks, because every string here
%% reaches C as a NUL terminated one and would otherwise name something shorter
%% than what was asked for.
hostile_arguments_are_refused_not_survived(Config) ->
    Env = proplists:get_value(env, Config),
    Path = tflite_beam_test_models:path(?MODEL),
    Model = model(Config, #{accelerators => [cpu]}),
    {ok, {Ins, _}} = ?A:io_sizes(Model),
    Good = [filled(1.0, N) || N <- Ins],

    ?assertMatch({error, _}, ?A:environment(<<"/tmp/a", 0, "b">>)),
    ?assertMatch({error, _}, ?A:new(Env, <<"a", 0, "b">>, #{})),
    ?assertMatch({error, _}, ?A:signatures(Env, <<"a", 0, "b">>)),
    ?assertMatch({error, _}, ?A:new(Env, Path, #{signature => <<"a", 0, "b">>})),

    %% an empty accelerator set selects nothing to run on
    ?assertMatch({error, _}, ?A:new(Env, Path, #{accelerators => []})),
    %% and an index the NIF cannot take is named here rather than there
    ?assertMatch({error, _}, ?A:new(Env, Path, #{signature => 4294967296})),

    ?assertMatch({error, _}, ?A:run(Model, [])),
    ?assertMatch({error, _}, ?A:run(Model, [improper | Good])),
    ?assertMatch({error, _}, ?A:run(Model, [1, 2, 3, 4])),
    ?assertMatch({error, _}, ?A:run(Model, lists:duplicate(1000, hd(Good)))),
    ?assertMatch({error, _}, ?A:run(Model, [binary:copy(<<0>>, 1000000) | tl(Good)])),

    %% A wrong option value is an ordinary mistake, so it answers like every
    %% sibling does rather than raising and making the caller catch one of them.
    [?assertMatch({error, _}, ?A:new(Env, Path, Bad))
     || Bad <- [#{accelerators => [nope]}, #{accelerators => 7},
                #{accelerators => []}, #{precision => nope},
                #{profile => perhaps}]],
    %% and a perfectly ordinary Unicode string is text, not a crash
    ?assertMatch({error, _}, ?A:new(Env, [28450, 23383], #{})),

    %% and after all of that the model still runs
    ?assertMatch({ok, _}, ?A:run(Model, Good)).

signatures_are_listed(Config) ->
    Env = proplists:get_value(env, Config),
    {ok, Keys} = ?A:signatures(Env, tflite_beam_test_models:path(?MODEL)),
    %% the fixture's actual key, so a stub that fabricates one does not pass
    ?assertEqual([<<"serving_default">>], Keys).

%% Naming the signature has to reach the same model that its index does, or the
%% lookup is decorative.
a_signature_can_be_named(Config) ->
    Env = proplists:get_value(env, Config),
    Path = tflite_beam_test_models:path(?MODEL),
    {ok, [Key | _]} = ?A:signatures(Env, Path),
    ByIndex = model(Config, #{accelerators => [cpu], signature => 0}),
    {ok, Named} = ?A:new(Env, Path, #{accelerators => [cpu], signature => Key}),
    ?assertEqual(?A:io_sizes(ByIndex), ?A:io_sizes(Named)),
    {ok, {Ins, _}} = ?A:io_sizes(ByIndex),
    Inputs = [filled(2.0, N) || N <- Ins],
    ?assertEqual(?A:run(ByIndex, Inputs), ?A:run(Named, Inputs)).

%% A signature index is not a subgraph index, and a model with one signature
%% cannot tell the two apart. This fixture has three, and they compute different
%% functions rather than merely having different shapes: the same input through
%% `add', `mul' and `sub' has to come back as three different answers. Counting
%% a subgraph instead of a signature, or ignoring the index, gets one answer
%% three times.
each_signature_runs_its_own_function(Config) ->
    Env = proplists:get_value(env, Config),
    Path = tflite_beam_test_models:path("multi_signature.tflite"),
    {ok, Keys} = ?A:signatures(Env, Path),
    ?assertEqual([<<"add">>, <<"mul">>, <<"sub">>], Keys),

    Answers = [begin
                   {ok, M} = ?A:new(Env, Path, #{accelerators => [cpu], signature => I}),
                   {ok, {Ins, _}} = ?A:io_sizes(M),
                   {ok, [Out]} = ?A:run(M, [filled(3.0, N) || N <- Ins]),
                   <<First:32/float-native, _/binary>> = Out,
                   First
               end || I <- lists:seq(0, length(Keys) - 1)],
    %% 3 + 3, 3 * 3, 3 - 3
    ?assertEqual([6.0, 9.0, 0.0], Answers),

    %% and naming them gets the same three, in the same order
    ByName = [begin
                  {ok, M} = ?A:new(Env, Path, #{accelerators => [cpu], signature => K}),
                  {ok, {Ins, _}} = ?A:io_sizes(M),
                  {ok, [Out]} = ?A:run(M, [filled(3.0, N) || N <- Ins]),
                  <<First:32/float-native, _/binary>> = Out,
                  First
              end || K <- Keys],
    ?assertEqual(Answers, ByName).

an_unknown_signature_is_refused(Config) ->
    Env = proplists:get_value(env, Config),
    Path = tflite_beam_test_models:path(?MODEL),
    %% the same call with a real key works, so the refusal is about the name
    {ok, [Key | _]} = ?A:signatures(Env, Path),
    ?assertMatch({ok, _}, ?A:new(Env, Path, #{accelerators => [cpu], signature => Key})),
    ?assertMatch({error, _}, ?A:new(Env, Path, #{accelerators => [cpu],
                                                 signature => <<"not a signature">>})),
    ?assertMatch({error, _}, ?A:new(Env, Path, #{accelerators => [cpu], signature => 99})).

%% An accelerator may leave the two metric entries of its definition null, and
%% every accelerator reachable here does, so this asks for the empty list rather
%% than an error. It exists to catch the API breaking, not to assert a number.
%% An accelerator may leave the two metric entries of its definition null, and
%% every accelerator reachable here does, so this asks for the empty list rather
%% than a number. `{ok, {Outputs, []}}' looks the same whether LiteRT was asked
%% or not, so the NIF counts the times it reached the call and this asks whether
%% the count moved. Collection has to bracket a real inference, which is why
%% this is a run rather than a standalone metrics call.
metrics_are_empty_rather_than_an_error(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    {ok, {Ins, _}} = ?A:io_sizes(Model),
    Inputs = [filled(1.0, N) || N <- Ins],

    {ok, {Outputs, Metrics}} = ?A:run_with_metrics(Model, Inputs),
    ?assertEqual([], Metrics),
    %% the run really ran: the same inputs through run/2 give the same answers
    ?assertEqual({ok, Outputs}, ?A:run(Model, Inputs)),

    case fault_injection() of
        false ->
            ok;
        true ->
            Before = tflite_beam_nif:nif_litert_call_count(),
            {ok, {_, []}} = ?A:run_with_metrics(Model, Inputs),
            {ok, {_, []}} = ?A:run_with_metrics(Model, Inputs, 1),
            ?assertEqual(Before + 2, tflite_beam_nif:nif_litert_call_count())
    end,

    %% a detail level below zero is a caller mistake, so the guard rejects it
    ?assertError(function_clause, ?A:run_with_metrics(Model, Inputs, -1)).

profile_is_empty_unless_asked_for(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    {ok, {Ins, _}} = ?A:io_sizes(Model),
    {ok, _} = ?A:run(Model, [filled(1.0, N) || N <- Ins]),
    ?assertEqual({ok, []}, ?A:profile(Model)).

profile_names_the_operators(Config) ->
    Model = model(Config, #{accelerators => [cpu], profile => true}),
    {ok, {Ins, _}} = ?A:io_sizes(Model),
    {ok, _} = ?A:run(Model, [filled(1.0, N) || N <- Ins]),
    {ok, Events} = ?A:profile(Model),
    ?assertNotEqual([], Events),
    {ok, Summary} = ?A:summarise_profile(Model),
    ?assertNotEqual([], Summary),
    %% every entry is a tag, an operator kind, a count and a total, the totals
    %% are ordered, and nothing that is not an operator got in
    Totals = [U || {_, _, _, U} <- Summary],
    ?assertEqual(lists:reverse(lists:sort(Totals)), Totals),
    ?assert(lists:all(fun({T, K, C, _}) ->
                          is_binary(T) andalso C > 0 andalso
                          lists:member(K, [operator, delegate_operator, delegate_profiled])
                      end, Summary)),
    %% the enclosing Invoke is an event but not an operator, so it must be in
    %% profile/1 and absent from the summary
    ?assert(lists:any(fun(E) -> maps:get(tag, E) =:= <<"Invoke">> end, Events)),
    ?assertEqual([], [X || X = {<<"Invoke">>, _, _, _} <- Summary]),
    %% Resetting empties it, and recording has to survive: LiteRT's own reset
    %% clears the profile buffer's enabled flag and does not put it back, so a
    %% test that only checks emptiness passes on a model that will never record
    %% again.
    ok = ?A:reset_profile(Model),
    ?assertEqual({ok, []}, ?A:summarise_profile(Model)),
    {ok, _} = ?A:run(Model, [filled(1.0, N) || N <- Ins]),
    {ok, AfterRerun} = ?A:summarise_profile(Model),
    ?assertNotEqual([], AfterRerun).

reset_profile_needs_profiling_on(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    ?assertMatch({error, _}, ?A:reset_profile(Model)).

%% The direct API now refuses a second concurrent caller rather than letting two
%% of them into LiteRT at once, because LiteRT says its compiled model API is
%% not verified for multithreading and the profile buffer under it says outright
%% that it is not thread safe. So the property to test is no longer "some
%% answers come back wrong", which required provoking a data race to observe;
%% it is that concurrent callers are turned away and that the server never is.
server_serialises_what_sharing_gets_wrong(Config) ->
    Env = proplists:get_value(env, Config),
    Path = tflite_beam_test_models:path(?MODEL),
    Model = model(Config, #{accelerators => [cpu]}),
    {ok, {Ins, _}} = ?A:io_sizes(Model),
    Inputs = [filled(3.0, N) || N <- Ins],

    %% A second caller is refused by this library before it reaches LiteRT, and
    %% the refusal has to be *ours*: without the lock LiteRT fails on its own,
    %% with a different message, so counting errors would pass either way.
    %%
    %% The collision is arranged rather than raced for. Two processes calling at
    %% once only overlap when there are two dirty schedulers to run them on, and
    %% on one they never do, so a racing test fails for a reason that has
    %% nothing to do with the code. The fault point holds the lock instead.
    %% Two dirty NIF calls cannot overlap on one dirty scheduler: the second
    %% waits in the scheduler queue rather than at the lock, and by the time it
    %% runs the first has finished. The refusal is real, the configuration
    %% simply cannot produce it, so this half is skipped there rather than made
    %% to look like a defect.
    case fault_injection() andalso erlang:system_info(dirty_cpu_schedulers) > 1 of
        false ->
            ok;
        true ->
            ok = tflite_beam_nif:nif_arm_fault(compiled_model_hold_lock),
            Holder = spawn(fun() -> ?A:run(Model, Inputs) end),
            HRef = monitor(process, Holder),
            timer:sleep(100),
            ?assertMatch({error, <<"compiled model is in use by another caller">>},
                         ?A:run(Model, Inputs)),
            receive {'DOWN', HRef, process, _, _} -> ok after 10000 -> ct:fail(holder_stuck) end
    end,

    %% Every worker feeds a value of its own, so a call that succeeded with
    %% somebody else's data is visible rather than discarded. Identical inputs
    %% would make a lock released too early, after the run but before the output
    %% copy, indistinguishable from a correct one.
    Pairs = [begin
                 In = [filled(V, N) || N <- Ins],
                 {ok, Out} = ?A:run(Model, In),
                 {In, Out}
             end || V <- [1.0, 5.0, 9.0, 13.0]],
    ?assertEqual(4, length(lists:usort([O || {_, O} <- Pairs]))),

    %% Direct: whatever got in must have got its own answer, and whatever was
    %% refused must have been refused by this library rather than by LiteRT
    %% failing underneath it.
    {DOk, DWrong, DErrs} = tally_keep_errors(fun(In) -> ?A:run(Model, In) end, Pairs, 20),
    ?assertEqual(0, DWrong),
    ?assert(DOk > 0),
    ?assert(lists:all(fun(E) -> E =:= <<"compiled model is in use by another caller">> end,
                      DErrs)),

    %% and through the server every one of them gets in and gets its own answer
    {ok, Server} = ?B:start(Env, Path, #{accelerators => [cpu]}),
    try
        {Ok, Wrong, Err} = tally(fun(In) -> ?B:run(Server, In) end, Pairs, 20),
        ?assertEqual({80, 0, 0}, {Ok, Wrong, Err})
    after
        ?B:stop(Server)
    end.

%% Like tally, but hands back the error messages rather than a count, so a
%% caller can insist they are the ones this library produces.
tally_keep_errors(Run, Wants, Rounds) ->
    Refs = [element(2, spawn_monitor(fun() ->
                exit({tally, count_keep_errors(Run, In, Want, Rounds)})
            end)) || {In, Want} <- Wants],
    lists:foldl(fun(Ref, {O, W, Es}) ->
        receive
            {'DOWN', Ref, process, _, {tally, {O2, W2, Es2}}} -> {O + O2, W + W2, Es2 ++ Es};
            {'DOWN', Ref, process, _, Other}                  -> ct:fail({worker_died, Other})
        after 60000 -> ct:fail(worker_timeout)
        end
    end, {0, 0, []}, Refs).

count_keep_errors(Run, In, Want, Rounds) ->
    lists:foldl(fun(_, {Ok, Wrong, Es}) ->
        case Run(In) of
            {ok, Want}   -> {Ok + 1, Wrong, Es};
            {ok, _}      -> {Ok, Wrong + 1, Es};
            {error, Why} -> {Ok, Wrong, [Why | Es]}
        end
    end, {0, 0, []}, lists:seq(1, Rounds)).

%% Returns the refusal messages, not a count, so a caller can tell whose they are.
fault_injection() ->
    case catch tflite_beam_nif:nif_arm_fault(none) of
        ok -> true;
        _  -> false
    end.

concurrent_refusals(Model, Inputs, Workers, Rounds) ->
    Refs = [element(2, spawn_monitor(fun() ->
                Rs = lists:foldl(fun(_, Acc) ->
                        case ?A:run(Model, Inputs) of
                            {ok, _}      -> Acc;
                            {error, Why} -> [Why | Acc]
                        end end, [], lists:seq(1, Rounds)),
                exit({refused, Rs})
            end)) || _ <- lists:seq(1, Workers)],
    lists:foldl(fun(Ref, Acc) ->
        receive
            {'DOWN', Ref, process, _, {refused, Rs}} -> Rs ++ Acc;
            {'DOWN', Ref, process, _, Other}         -> ct:fail({worker_died, Other})
        after 60000 -> ct:fail(worker_timeout)
        end
    end, [], Refs).

tally(Run, Wants, Rounds) ->
    Refs = [element(2, spawn_monitor(fun() ->
                exit({tally, count(Run, In, Want, Rounds)})
            end)) || {In, Want} <- Wants],
    lists:foldl(fun(Ref, {O, W, E}) ->
        receive
            {'DOWN', Ref, process, _, {tally, {O2, W2, E2}}} -> {O + O2, W + W2, E + E2};
            {'DOWN', Ref, process, _, _}                     -> {O, W, E + Rounds}
        after 60000 -> {O, W, E + Rounds}
        end
    end, {0, 0, 0}, Refs).



%% Ownership is opt-in, so an unclaimed model has to stay usable from anywhere.
%% Without this the next two tests would pass on a resource that refused
%% everybody.
an_unclaimed_model_is_open_to_every_process(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    Inputs = inputs_for(Model),
    ?assertEqual(undefined, ?A:controlling_process(Model)),
    ?assertMatch({ok, _}, run_in_another_process(Model, Inputs)).

a_claimed_model_refuses_other_processes(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    Inputs = inputs_for(Model),
    ?assertMatch({ok, _}, run_in_another_process(Model, Inputs)),
    ok = ?A:controlling_process(Model, self()),
    ?assertEqual({ok, self()}, ?A:controlling_process(Model)),
    %% the owner still runs it
    ?assertMatch({ok, _}, run_here(Model, Inputs)),
    %% and nobody else does
    ?assertMatch({error, _}, run_in_another_process(Model, Inputs)).

%% A model whose owner has died would otherwise be unusable for ever.
a_claim_dies_with_its_process(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    Inputs = inputs_for(Model),
    Self = self(),
    Owner = spawn(fun() ->
        ok = ?A:controlling_process(Model, self()),
        Self ! claimed,
        receive stop -> ok end
    end),
    receive claimed -> ok after 5000 -> ct:fail(claim_timeout) end,
    ?assertMatch({error, _}, run_here(Model, Inputs)),
    Ref = monitor(process, Owner),
    Owner ! stop,
    receive {'DOWN', Ref, process, _, _} -> ok after 5000 -> ct:fail(owner_timeout) end,
    %% the getter notices the death by itself, without a guarded operation
    %% having to run first and clear the claim
    ?assertEqual(undefined, ?A:controlling_process(Model)),
    ?assertMatch({ok, _}, run_here(Model, Inputs)).

%% with/2 hands the reference to a callback, and a callback that keeps it must
%% not be able to use it from elsewhere afterwards.
the_server_claims_what_with_hands_out(Config) ->
    Env = proplists:get_value(env, Config),
    {ok, Server} = ?B:start(Env, tflite_beam_test_models:path(?MODEL),
                            #{accelerators => [cpu]}),
    try
        {Escaped, Inputs} = ?B:with(Server, fun(M) -> {M, inputs_for(M)} end),
        ?assertEqual({ok, Server}, ?A:controlling_process(Escaped)),
        ?assertMatch({error, _}, run_here(Escaped, Inputs))
    after
        ?B:stop(Server)
    end.

%% The sizes are read while this process may still read them, because io_sizes
%% goes through the same ownership check that run does: a non-owner is refused
%% everything, not only the run.
inputs_for(Model) ->
    {ok, {Ins, _}} = ?A:io_sizes(Model),
    [filled(1.0, N) || N <- Ins].

run_here(Model, Inputs) ->
    ?A:run(Model, Inputs).

run_in_another_process(Model, Inputs) ->
    Self = self(),
    spawn(fun() -> Self ! {result, ?A:run(Model, Inputs)} end),
    receive {result, R} -> R after 30000 -> {error, <<"timeout">>} end.

count(Run, In, Want, Rounds) ->
    lists:foldl(fun(_, {Ok, Wrong, Err}) ->
        case catch Run(In) of
            {ok, Want} -> {Ok + 1, Wrong, Err};
            {ok, _}    -> {Ok, Wrong + 1, Err};
            _          -> {Ok, Wrong, Err + 1}
        end
    end, {0, 0, 0}, lists:seq(1, Rounds)).

spawn_owner(Model) ->
    Self = self(),
    Pid = spawn(fun() ->
        ok = ?A:controlling_process(Model, self()),
        Self ! claimed,
        receive release -> ok end
    end),
    receive claimed -> ok after 5000 -> ct:fail(claim_timeout) end,
    Pid.

release_owner(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! release,
    receive {'DOWN', Ref, process, _, _} -> ok after 5000 -> ok end.


%% An argument the direct module refuses with a guard would, if it reached the
%% server's handle_call, raise there and take the model down with the server. A
%% caller mistake must cost the call and nothing else.
a_bad_detail_level_does_not_take_the_server_down(Config) ->
    Env = proplists:get_value(env, Config),
    {ok, Server} = ?B:start(Env, tflite_beam_test_models:path(?MODEL),
                            #{accelerators => [cpu]}),
    try
        {ok, {Ins, _}} = ?B:io_sizes(Server),
        Inputs = [filled(1.0, N) || N <- Ins],
        ?assertMatch({ok, {_, []}}, ?B:run_with_metrics(Server, Inputs)),

        [?assertError(function_clause, ?B:run_with_metrics(Server, Inputs, Bad))
         || Bad <- [-1, 4294967296, not_a_number]],

        %% the server survived every one of them and still works
        ?assert(is_process_alive(Server)),
        ?assertMatch({ok, _}, ?B:run(Server, Inputs))
    after
        case is_process_alive(Server) of
            true -> ?B:stop(Server);
            false -> ok
        end
    end.
