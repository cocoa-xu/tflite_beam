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
    signatures_are_listed/1,
    a_signature_can_be_named/1,
    each_signature_gets_its_own_shape/1,
    an_unknown_signature_is_refused/1,
    metrics_are_empty_rather_than_an_error/1,
    profile_is_empty_unless_asked_for/1,
    profile_names_the_operators/1,
    reset_profile_needs_profiling_on/1,
    server_serialises_what_sharing_gets_wrong/1,
    an_unclaimed_model_is_open_to_every_process/1,
    a_claimed_model_refuses_other_processes/1,
    a_claim_dies_with_its_process/1,
    the_server_claims_what_with_hands_out/1
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
        signatures_are_listed,
        a_signature_can_be_named,
        each_signature_gets_its_own_shape,
        an_unknown_signature_is_refused,
        metrics_are_empty_rather_than_an_error,
        profile_is_empty_unless_asked_for,
        profile_names_the_operators,
        reset_profile_needs_profiling_on,
        server_serialises_what_sharing_gets_wrong,
        an_unclaimed_model_is_open_to_every_process,
        a_claimed_model_refuses_other_processes,
        a_claim_dies_with_its_process,
        the_server_claims_what_with_hands_out
    ].

%% The Erlang stubs exist whatever the library was built with; what is missing
%% without the LiteRT API is the NIF behind them, and calling one then raises
%% rather than returning an error tuple.
init_per_suite(Config) ->
    case catch ?A:platform_support() of
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

signatures_are_listed(Config) ->
    Env = proplists:get_value(env, Config),
    {ok, Keys} = ?A:signatures(Env, tflite_beam_test_models:path(?MODEL)),
    ?assertNotEqual([], Keys),
    ?assert(lists:all(fun is_binary/1, Keys)).

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
%% cannot tell the two apart. No fixture here has more than one: the smallest
%% multi-signature model in LiteRT's own test data is 49MB with 23MB tensors,
%% which is not something to carry in this repository. So this runs against a
%% model named by TFLITE_BEAM_MULTI_SIGNATURE_MODEL and skips without one.
%%
%% Verified by hand on 2026-08-27 against LiteRT's model_magic_test.tflite: its
%% eight signatures produced output counts of 33 and 32 and first input sizes
%% between 32768 and 23116544, all different, which is what counting the
%% signature rather than its subgraph gets you.
each_signature_gets_its_own_shape(Config) ->
    case os:getenv("TFLITE_BEAM_MULTI_SIGNATURE_MODEL") of
        false ->
            {skip, "set TFLITE_BEAM_MULTI_SIGNATURE_MODEL to a model with several signatures"};
        Path ->
            Env = proplists:get_value(env, Config),
            {ok, Keys} = ?A:signatures(Env, Path),
            ?assert(length(Keys) > 1),
            Shapes = [begin
                          {ok, M} = ?A:new(Env, Path, #{accelerators => [cpu], signature => I}),
                          {ok, Sizes} = ?A:io_sizes(M),
                          Sizes
                      end || I <- lists:seq(0, length(Keys) - 1)],
            %% counting the subgraph instead would hand several signatures the
            %% same shape; these have to differ
            ?assert(length(lists:usort(Shapes)) > 1)
    end.

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
metrics_are_empty_rather_than_an_error(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    ?assertEqual({ok, []}, ?A:metrics(Model)),
    ?assertEqual({ok, []}, ?A:metrics(Model, 2)),
    %% An empty list from a NIF that never called LiteRT would pass the two
    %% assertions above, so this asks the same NIF about a model it cannot use.
    %% Reaching LiteRT is the only way to answer that differently.
    Other = spawn_owner(Model),
    try
        ?assertMatch({error, _}, ?A:metrics(Model))
    after
        release_owner(Other)
    end,
    %% a detail level below zero is a caller mistake, so the guard rejects it
    %% rather than the NIF being asked about it
    ?assertError(function_clause, ?A:metrics(Model, -1)).

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

    %% A second caller is refused by this library before it reaches LiteRT.
    %% Counting refusals is not enough to show that: without the lock LiteRT
    %% fails on its own, with a different message, and a test that counted
    %% errors would pass either way. So the refusal has to be *ours*.
    Refusals = concurrent_refusals(Model, Inputs, 8, 20),
    Ours = [R || R <- Refusals, binary:match(R, <<"in use by another caller">>) =/= nomatch],
    ?assertNotEqual([], Ours),

    %% and every call that did get in returned this input's own answer
    {ok, Want} = ?A:run(Model, Inputs),
    {ok, Server} = ?B:start(Env, Path, #{accelerators => [cpu]}),
    try
        {Ok, Wrong, Err} = tally(fun(In) -> ?B:run(Server, In) end,
                                 [{Inputs, Want} || _ <- lists:seq(1, 4)], 20),
        ?assertEqual({80, 0, 0}, {Ok, Wrong, Err})
    after
        ?B:stop(Server)
    end.

%% Returns the refusal messages, not a count, so a caller can tell whose they are.
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
    ?assertMatch({ok, _}, run_here(Model, Inputs)),
    ?assertEqual(undefined, ?A:controlling_process(Model)).

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
