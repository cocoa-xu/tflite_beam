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
    availability_is_answerable_without_a_call/1,
    hostile_arguments_are_refused_not_survived/1,
    signatures_are_listed/1,
    a_signature_can_be_named/1,
    each_signature_runs_its_own_function/1,
    an_unknown_signature_is_refused/1,
    metrics_are_empty_rather_than_an_error/1,
    profile_is_empty_unless_asked_for/1,
    profile_names_the_operators/1,
    pending_events_tracks_the_backlog/1,
    reset_profile_needs_profiling_on/1,
    server_serialises_what_sharing_gets_wrong/1,
    an_unclaimed_model_is_open_to_every_process/1,
    a_claimed_model_refuses_other_processes/1,
    a_claim_dies_with_its_process/1,
    the_server_claims_what_with_hands_out/1,
    a_bad_detail_level_does_not_take_the_server_down/1,
    a_raising_callback_costs_the_call_not_the_model/1,
    a_model_larger_than_its_limit_is_refused/1,
    a_full_queue_is_refused_rather_than_grown/1,
    an_isolated_model_runs_and_its_death_is_survivable/1,
    every_forwarded_call_reaches_the_model/1,
    a_recompiled_callback_module_is_carried_over/1
]).

-define(A, tflite_beam_litert_compiled_model).
-define(B, tflite_beam_litert_compiled_model_server).
-define(I, tflite_beam_litert_compiled_model_isolated).
-define(MODEL, "multi_add.bin").

all() ->
    [
        environment_is_created,
        compiled_model_runs,
        compiled_model_refuses_a_wrong_sized_input,
        compiled_model_refuses_the_wrong_number_of_inputs,
        platform_support_is_reported,
    availability_is_answerable_without_a_call,
        hostile_arguments_are_refused_not_survived,
        signatures_are_listed,
        a_signature_can_be_named,
        each_signature_runs_its_own_function,
        an_unknown_signature_is_refused,
        metrics_are_empty_rather_than_an_error,
        profile_is_empty_unless_asked_for,
        profile_names_the_operators,
        pending_events_tracks_the_backlog,
        reset_profile_needs_profiling_on,
        server_serialises_what_sharing_gets_wrong,
        an_unclaimed_model_is_open_to_every_process,
        a_claimed_model_refuses_other_processes,
        a_claim_dies_with_its_process,
        the_server_claims_what_with_hands_out,
        a_bad_detail_level_does_not_take_the_server_down,
        a_raising_callback_costs_the_call_not_the_model,
        a_model_larger_than_its_limit_is_refused,
        a_full_queue_is_refused_rather_than_grown,
        an_isolated_model_runs_and_its_death_is_survivable,
        every_forwarded_call_reaches_the_model,
        a_recompiled_callback_module_is_carried_over
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
        {ok, Support} when is_map(Support) ->
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
    {ok, Support} = ?A:platform_support(),
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
%% available/0 has to agree with what every other function does, or it is worse
%% than not having it: a build where it says true and the calls say "not
%% compiled" would send a caller looking in the wrong place entirely.
availability_is_answerable_without_a_call(Config) ->
    ?assert(?A:available()),
    ?assertMatch({ok, Map} when is_map(Map), ?A:platform_support()),
    Env = proplists:get_value(env, Config),
    ?assertMatch({ok, _}, ?A:signatures(Env, tflite_beam_test_models:path(?MODEL))).

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
    %% and a signature that is neither an index nor a key is refused like every
    %% other option of the wrong kind, where it was a function_clause
    ?assertMatch({error, _}, ?A:new(Env, Path, #{signature => serving_default})),

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
    %% the bound really bounds, and zero means all of them
    {ok, Few} = ?A:profile(Model, 3),
    ?assertEqual(3, length(Few)),
    ?assertEqual(lists:sublist(Events, length(Events) - 2, 3), Few),
    ?assertEqual({ok, Events}, ?A:profile(Model, 0)),
    ?assertEqual({ok, Events}, ?A:profile(Model, length(Events) + 100)),
    {ok, Summary} = ?A:summarise_profile(Model),
    ?assertNotEqual([], Summary),
    %% every entry names a tag, an operator kind, a count and a total, the
    %% totals are ordered, and nothing that is not an operator got in
    Totals = [U || #{us := U} <- Summary],
    ?assertEqual(lists:reverse(lists:sort(Totals)), Totals),
    ?assert(lists:all(fun(#{tag := T, kind := K, count := C}) ->
                          is_binary(T) andalso C > 0 andalso
                          lists:member(K, [operator, delegate_operator, delegate_profiled])
                      end, Summary)),
    %% the enclosing Invoke is an event but not an operator, so it must be in
    %% profile/1 and absent from the summary
    ?assert(lists:any(fun(E) -> maps:get(tag, E) =:= <<"Invoke">> end, Events)),
    ?assertEqual([], [X || X = #{tag := <<"Invoke">>} <- Summary]),
    %% The event types are named, not LiteRT's raw numbers. This also serves as
    %% a tripwire: an upstream bump that adds an event type arrives here as an
    %% integer and fails this line, which is the point at which the new type
    %% wants naming rather than the point a caller discovers a bare number.
    ?assert(lists:all(fun(#{type := T, source := S}) ->
                          is_atom(T) andalso is_atom(S)
                      end, Events)),
    ?assert(lists:member(litert, [maps:get(source, E) || E <- Events])
            orelse lists:member(tflite_interpreter, [maps:get(source, E) || E <- Events])),
    %% Resetting empties it, and recording has to survive: LiteRT's own reset
    %% clears the profile buffer's enabled flag and does not put it back, so a
    %% test that only checks emptiness passes on a model that will never record
    %% again.
    ok = ?A:reset_profile(Model),
    ?assertEqual({ok, []}, ?A:summarise_profile(Model)),
    {ok, _} = ?A:run(Model, [filled(1.0, N) || N <- Ins]),
    {ok, AfterRerun} = ?A:summarise_profile(Model),
    ?assertNotEqual([], AfterRerun).

%% The size of the copy profile/2 makes is set by this, not by the limit given
%% to it, so the number has to be reachable without making the copy. Asserting
%% only "it is a number" would pass against a stub returning zero, so what is
%% checked is that it moves with the events and that the limit does not move it.
pending_events_tracks_the_backlog(Config) ->
    Unprofiled = model(Config, #{accelerators => [cpu]}),
    ?assertEqual({ok, 0}, ?A:pending_events(Unprofiled)),

    Model = model(Config, #{accelerators => [cpu], profile => true}),
    {ok, {Ins, _}} = ?A:io_sizes(Model),
    Inputs = [filled(1.0, N) || N <- Ins],
    {ok, _} = ?A:run(Model, Inputs),
    {ok, AfterOne} = ?A:pending_events(Model),
    ?assert(AfterOne > 0),

    {ok, _} = ?A:run(Model, Inputs),
    {ok, AfterTwo} = ?A:pending_events(Model),
    ?assert(AfterTwo > AfterOne),

    %% reading a bounded slice must not drain the backlog, because the copy is
    %% still whole-backlog sized on the next call
    {ok, _} = ?A:profile(Model, 1),
    ?assertEqual({ok, AfterTwo}, ?A:pending_events(Model)),

    ok = ?A:reset_profile(Model),
    ?assertEqual({ok, 0}, ?A:pending_events(Model)).

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


%% A callback belongs to whoever wrote it and may raise. Losing a compiled model,
%% and every caller queued behind it, to somebody else's mistake is not a
%% reasonable price, so the failure comes back to the caller that caused it.
%% The same goes for an out of range limit reaching the server through profile/2.
a_raising_callback_costs_the_call_not_the_model(Config) ->
    Env = proplists:get_value(env, Config),
    {ok, Server} = ?B:start(Env, tflite_beam_test_models:path(?MODEL),
                            #{accelerators => [cpu]}),
    try
        ?assertMatch({error, _}, ?B:with(Server, fun(_) -> error(boom) end)),
        ?assertMatch({error, _}, ?B:with(Server, fun(_) -> throw(nope) end)),
        ?assertMatch({error, _}, ?B:with(Server, fun(_) -> exit(gone) end)),
        ?assert(is_process_alive(Server)),

        [?assertError(function_clause, ?B:profile(Server, Bad))
         || Bad <- [-1, 4294967296, not_a_number]],
        ?assert(is_process_alive(Server)),

        %% and the model is still there and still works
        {ok, {Ins, _}} = ?B:io_sizes(Server),
        ?assertMatch({ok, _}, ?B:run(Server, [filled(1.0, N) || N <- Ins]))
    after
        case is_process_alive(Server) of
            true -> ?B:stop(Server);
            false -> ok
        end
    end.


%% What LiteRT allocates for a model is invisible to the emulator, so a caller
%% whose model paths come from elsewhere wants to say no before the read.
a_model_larger_than_its_limit_is_refused(Config) ->
    Env = proplists:get_value(env, Config),
    Path = tflite_beam_test_models:path(?MODEL),
    ?assertMatch({ok, _}, ?A:new(Env, Path, #{})),
    ?assertMatch({ok, _}, ?A:new(Env, Path, #{max_model_bytes => 0})),
    ?assertMatch({ok, _}, ?A:new(Env, Path, #{max_model_bytes => 10000000})),
    ?assertMatch({error, _}, ?A:new(Env, Path, #{max_model_bytes => 10})),
    ?assertMatch({error, _}, ?A:new(Env, Path, #{max_model_bytes => big})).

%% An inference runs inside handle_call, so callers queue behind it, and one
%% whose call timed out has stopped waiting without its request stopping. Under
%% overload that queue is where the memory goes, so past a bound the server says
%% no instead of growing.
a_full_queue_is_refused_rather_than_grown(Config) ->
    Env = proplists:get_value(env, Config),
    {ok, Server} = ?B:start(Env, tflite_beam_test_models:path(?MODEL),
                            #{accelerators => [cpu], max_queue => 4}),
    try
        {ok, {Ins, _}} = ?B:io_sizes(Server),
        Inputs = [filled(1.0, N) || N <- Ins],
        Self = self(),
        [spawn(fun() -> Self ! {done, catch ?B:run(Server, Inputs, 30000)} end)
         || _ <- lists:seq(1, 40)],
        {Ok, Refused} = lists:foldl(
            fun(_, {O, R}) ->
                receive
                    {done, {ok, _}} -> {O + 1, R};
                    {done, {error, _}} -> {O, R + 1};
                    {done, _} -> {O, R}
                after 30000 -> {O, R}
                end
            end, {0, 0}, lists:seq(1, 40)),
        ?assert(Refused > 0),
        ?assert(Ok > 0),
        ?assertEqual(40, Ok + Refused),
        %% and the server is still there to serve the next caller
        ?assert(is_process_alive(Server)),
        ?assertMatch({ok, _}, ?B:run(Server, Inputs))
    after
        ?B:stop(Server)
    end.


%% Both process layers are a list of one line forwards, and a one line forward is
%% what a typo survives: a wrong atom reaches the far side and comes back as an
%% error nobody reads. Eleven of them had no test at all. Calling each once, and
%% asserting the shape rather than the value, is the whole of it.
every_forwarded_call_reaches_the_model(Config) ->
    Env = proplists:get_value(env, Config),
    Path = tflite_beam_test_models:path(?MODEL),
    Opts = #{accelerators => [cpu], profile => true},

    {ok, Server} = ?B:start_link(Env, Path, Opts),
    {ok, {Ins, _}} = ?B:io_sizes(Server),
    Inputs = [filled(1.0, N) || N <- Ins],
    ?assertMatch({ok, [_ | _]}, ?B:run(Server, Inputs)),
    ?assertMatch({ok, B} when is_boolean(B), ?B:fully_accelerated(Server)),
    ?assertMatch({ok, {_, _}}, ?B:run_with_metrics(Server, Inputs)),
    ?assertMatch({ok, L} when is_list(L), ?B:profile(Server)),
    ?assertMatch({ok, N} when is_integer(N), ?B:pending_events(Server)),
    ?assertMatch({ok, L2} when is_list(L2), ?B:summarise_profile(Server)),
    ?assertEqual(ok, ?B:reset_profile(Server)),
    ?assertEqual({ok, 0}, ?B:pending_events(Server)),
    ?assertMatch({ok, {_, _}}, ?B:with(Server, fun(M) -> ?A:io_sizes(M) end)),
    ?B:stop(Server),

    case ?I:start(Opts#{model_path => Path}) of
        {error, Reason} ->
            {skip, {cannot_start_an_isolated_node, Reason}};
        {ok, Iso} ->
            try
                ?assertMatch({ok, {_, _}}, ?I:io_sizes(Iso)),
                ?assertMatch({ok, [_ | _]}, ?I:run(Iso, Inputs)),
                ?assertMatch({ok, B2} when is_boolean(B2), ?I:fully_accelerated(Iso)),
                ?assertMatch({ok, {_, _}}, ?I:run_with_metrics(Iso, Inputs)),
                ?assertMatch({ok, L3} when is_list(L3), ?I:profile(Iso)),
                ?assertMatch({ok, N2} when is_integer(N2), ?I:pending_events(Iso)),
                ?assertMatch({ok, L4} when is_list(L4), ?I:summarise_profile(Iso)),
                ?assertEqual(ok, ?I:reset_profile(Iso)),
                ?assertMatch({ok, Node} when is_atom(Node), ?I:node_of(Iso))
            after
                ?I:stop(Iso)
            end
    end.

%% nodedown does not arrive the instant the node is halted, so the refusal is
%% waited for rather than assumed. Failing here rather than timing out says the
%% node death never reached the isolating process at all.
wait_for_isolated_down(_Server, _Inputs, 0) ->
    ct:fail(the_isolated_model_never_noticed_its_node_died);
wait_for_isolated_down(Server, Inputs, Tries) ->
    case tflite_beam_litert_compiled_model_isolated:run(Server, Inputs) of
        {error, <<"the isolated model's node went down">>} -> ok;
        _ -> timer:sleep(100), wait_for_isolated_down(Server, Inputs, Tries - 1)
    end.

%% The whole point of the isolated variant: a model that dies takes its node and
%% nothing else. Killing the node stands in for the segmentation fault this
%% exists to survive, because a real one cannot be arranged from Erlang.
%%
%% start_link, deliberately. With start/1 there is no link, so the caller would
%% survive whatever the isolating process did and the test would pass against a
%% version that exits on nodedown and takes every linked caller with it, which
%% is what this used to do.
%%
%% Trapping exits is what makes the link observable rather than fatal. A CI
%% machine that cannot start distribution makes init return {stop, Reason}, and
%% start_link then both returns {error, Reason} and sends an exit signal; without
%% this the case died on the signal before it could reach the skip below, which
%% is how a machine with no distribution reported a failure rather than a skip.
an_isolated_model_runs_and_its_death_is_survivable(Config) ->
    process_flag(trap_exit, true),
    Path = tflite_beam_test_models:path(?MODEL),
    case tflite_beam_litert_compiled_model_isolated:start_link(
             #{model_path => Path, accelerators => [cpu]}) of
        {error, Reason} ->
            {skip, {cannot_start_an_isolated_node, Reason}};
        {ok, Server} ->
            ?I = tflite_beam_litert_compiled_model_isolated,
            {ok, Node} = ?I:node_of(Server),
            ?assertNotEqual(node(), Node),

            %% with/2 sends the callback to the node that owns the model and
            %% applies it there. A reference cannot cross, so this is the only
            %% way that call can mean anything, and it has to be exercised.
            {ok, {WIns, _}} = ?I:with(Server, fun(M) -> ?A:io_sizes(M) end),
            ?assert(is_list(WIns)),

            %% and a callback whose module the peer cannot be given is refused in
            %% words rather than passed on as a bare undef. A module compiled
            %% into memory and never written out is the position an ExUnit case
            %% or a mix script is in: code:get_object_code/1 has nothing to send.
            EphemeralMod = list_to_atom("no_file_" ++ integer_to_list(erlang:unique_integer([positive]))),
            {ok, EphemeralMod, Beam} = compile:forms(
                [{attribute, 1, module, EphemeralMod},
                 {attribute, 2, export, [{cb, 1}]},
                 {function, 3, cb, 1, [{clause, 3, [{var, 3, '_'}], [], [{atom, 3, ok}]}]}],
                [binary]),
            {module, EphemeralMod} = code:load_binary(EphemeralMod, "", Beam),
            ?assertEqual(error, code:get_object_code(EphemeralMod)),
            {error, Why} = ?I:with(Server, fun EphemeralMod:cb/1),
            ?assertNotEqual(nomatch, string:find(Why, <<"no compiled file to send">>)),

            {ok, {Ins, _}} = ?I:io_sizes(Server),
            Inputs = [filled(1.0, N) || N <- Ins],
            {ok, Outputs} = ?I:run(Server, Inputs),
            %% and it computes what the in-process one does
            ?assertEqual({ok, Outputs},
                         ?A:run(model(Config, #{accelerators => [cpu]}), Inputs)),

            rpc:cast(Node, erlang, halt, [1]),
            wait_for_isolated_down(Server, Inputs, 100),

            %% the calling process is still here, which is the point, and it is
            %% linked, so this is only true if the isolating process did not exit
            ?assert(is_process_alive(self())),
            ?assert(is_process_alive(Server)),
            ?assertMatch({error, <<"the isolated model's node went down">>},
                         ?I:run(Server, Inputs)),
            %% still able to say which node it was, which is what an error
            %% report wants
            ?assertEqual({ok, Node}, ?I:node_of(Server)),
            ?I:stop(Server),

            %% and a replacement starts, which is what a supervisor would do
            {ok, Replacement} = ?I:start(#{model_path => Path, accelerators => [cpu]}),
            try ?assertMatch({ok, _}, ?I:run(Replacement, Inputs))
            after ?I:stop(Replacement) end
    end.

%% A closure carries the version of the module that made it, so recompiling that
%% module here left the isolated node holding the older copy and the callback
%% arrived as a badfun, reported against this library's own file and line rather
%% than the caller's module. Having the module reachable there is not the same as
%% having the same one.
a_recompiled_callback_module_is_carried_over(Config) ->
    process_flag(trap_exit, true),
    Dir = ?config(priv_dir, Config),
    Source = filename:join(Dir, "skewed_callback.erl"),
    Write = fun(Body) ->
        ok = file:write_file(Source, iolist_to_binary(
            ["-module(skewed_callback).\n-export([make_fun/0]).\n"
             "make_fun() -> fun(_Model) -> ", Body, " end.\n"])),
        {ok, skewed_callback} = compile:file(Source, [{outdir, Dir}]),
        code:purge(skewed_callback),
        {module, skewed_callback} = code:load_abs(filename:join(Dir, "skewed_callback"))
    end,
    true = code:add_patha(Dir),

    Write("first"),
    Path = tflite_beam_test_models:path(?MODEL),
    case tflite_beam_litert_compiled_model_isolated:start_link(
             #{model_path => Path, accelerators => [cpu]}) of
        {error, Reason} ->
            {skip, {cannot_start_an_isolated_node, Reason}};
        {ok, Server} ->
            I = tflite_beam_litert_compiled_model_isolated,
            try
                %% the peer loads it from the shared code path, as it always did
                ?assertEqual(first, I:with(Server, skewed_callback:make_fun())),

                %% now this node has a version the peer does not
                Write("second"),
                ?assertEqual(second, I:with(Server, skewed_callback:make_fun()))
            after
                gen_server:stop(Server),
                code:purge(skewed_callback),
                code:delete(skewed_callback),
                code:del_path(Dir)
            end
    end.
