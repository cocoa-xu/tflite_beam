%% @doc LiteRT's compiled model, the profiler that only it has, and the process
%% that keeps two callers from sharing its buffers. Skipped whole when the
%% library was built without the LiteRT API.
-module(tflite_beam_litert_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, init_per_suite/1]).
-export([
    environment_is_created/1,
    compiled_model_runs/1,
    compiled_model_refuses_a_wrong_sized_input/1,
    compiled_model_refuses_the_wrong_number_of_inputs/1,
    profile_is_empty_unless_asked_for/1,
    profile_names_the_operators/1,
    reset_profile_needs_profiling_on/1,
    server_serialises_what_sharing_gets_wrong/1
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
        profile_is_empty_unless_asked_for,
        profile_names_the_operators,
        reset_profile_needs_profiling_on,
        server_serialises_what_sharing_gets_wrong
    ].

%% The Erlang stubs exist whatever the library was built with; what is missing
%% without the LiteRT API is the NIF behind them, and calling one then raises
%% rather than returning an error tuple.
init_per_suite(Config) ->
    case catch ?A:environment() of
        {ok, Env} ->
            [{env, Env} | Config];
        {error, Reason} ->
            {skip, {litert_environment_failed, Reason}};
        _ ->
            {skip, "built without TFLITE_BEAM_ENABLE_LITERT_API"}
    end.

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
    %% every entry is a tag, a count and a total, and the totals are ordered
    Totals = [U || {_, _, U} <- Summary],
    ?assertEqual(lists:reverse(lists:sort(Totals)), Totals),
    ?assert(lists:all(fun({T, C, _}) -> is_binary(T) andalso C > 0 end, Summary)),
    %% and resetting really does empty it
    ok = ?A:reset_profile(Model),
    {ok, After} = ?A:summarise_profile(Model),
    ?assertEqual([], After).

reset_profile_needs_profiling_on(Config) ->
    Model = model(Config, #{accelerators => [cpu]}),
    ?assertMatch({error, _}, ?A:reset_profile(Model)).

%% Four processes, four different inputs, each checking it gets its own answer.
%% Sharing one model directly is expected to lose some of them; the server is
%% expected to lose none. The first half is the negative control: without it a
%% passing second half would say nothing.
server_serialises_what_sharing_gets_wrong(Config) ->
    Env = proplists:get_value(env, Config),
    Path = tflite_beam_test_models:path(?MODEL),
    Model = model(Config, #{accelerators => [cpu]}),
    {ok, {Ins, _}} = ?A:io_sizes(Model),

    Values = [1.0, 5.0, 9.0, 13.0],
    Wants = [begin
                 In = [filled(V, N) || N <- Ins],
                 {ok, Out} = ?A:run(Model, In),
                 {In, Out}
             end || V <- Values],
    ?assertEqual(length(Values), length(lists:usort([O || {_, O} <- Wants]))),

    Rounds = 25,
    Shared = tally(fun(In) -> ?A:run(Model, In) end, Wants, Rounds),
    {ok, Server} = ?B:start(Env, Path, #{accelerators => [cpu]}),
    try
        Served = tally(fun(In) -> ?B:run(Server, In) end, Wants, Rounds),
        Total = length(Values) * Rounds,
        {SharedRight, _, _} = Shared,
        ?assert(SharedRight < Total),
        ?assertEqual({Total, 0, 0}, Served)
    after
        ?B:stop(Server)
    end.

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

count(Run, In, Want, Rounds) ->
    lists:foldl(fun(_, {Ok, Wrong, Err}) ->
        case catch Run(In) of
            {ok, Want} -> {Ok + 1, Wrong, Err};
            {ok, _}    -> {Ok, Wrong + 1, Err};
            _          -> {Ok, Wrong, Err + 1}
        end
    end, {0, 0, 0}, lists:seq(1, Rounds)).
