%% @doc The delegate resource and the attachment point, as far as they go
%% without a delegate constructor -- this phase ships none, so every case here
%% is about the boundary rather than about a delegate doing its job. The cases
%% that need a real one belong with the constructors that create them.
-module(tflite_beam_delegate_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    available_is_a_list/1,
    add_delegate_rejects_a_non_delegate/1,
    builder_survives_a_rejected_delegate/1,
    on_decline_default_is_error/1,
    on_decline_accepts_fallback/1,
    on_decline_rejects_unknown_value/1,
    unknown_option_is_rejected/1,
    xnnpack_is_compiled_in/1,
    xnnpack_rejects_unknown_flag_and_option/1,
    xnnpack_explicit_applies_at_build/1,
    default_delegates_opt_out/1,
    default_path_matches_old_default/1,
    set_num_threads_reaches_the_default_delegate/1,
    xnnpack_num_threads/1,
    xnnpack_weight_cache_file_path/1,
    delegate_outlives_builder/1,
    delegate_two_interpreters/1,
    declining_delegate_is_an_error_by_default/1,
    declining_delegate_falls_back/1,
    fallback_is_not_a_blanket_catch/1
]).

-define(FILLED(V), binary:copy(<<V:32/float-native>>, 1 * 8 * 8 * 3)).
-define(INPUTS, [?FILLED(1.0), ?FILLED(2.0), ?FILLED(3.0), ?FILLED(4.0)]).

all() ->
    [
        available_is_a_list,
        add_delegate_rejects_a_non_delegate,
        builder_survives_a_rejected_delegate,
        on_decline_default_is_error,
        on_decline_accepts_fallback,
        on_decline_rejects_unknown_value,
        unknown_option_is_rejected,
        xnnpack_is_compiled_in,
        xnnpack_rejects_unknown_flag_and_option,
        xnnpack_explicit_applies_at_build,
        default_delegates_opt_out,
        default_path_matches_old_default,
        set_num_threads_reaches_the_default_delegate,
        xnnpack_num_threads,
        xnnpack_weight_cache_file_path,
        delegate_outlives_builder,
        delegate_two_interpreters,
        declining_delegate_is_an_error_by_default,
        declining_delegate_falls_back,
        fallback_is_not_a_blanket_catch
    ].

%% Compile-time facts only, so it answers on every target and loads nothing.
available_is_a_list(_Config) ->
    Available = tflite_beam_delegate:available(),
    ?assert(is_list(Available)),
    ?assert(lists:all(fun is_atom/1, Available)).

%% An interpreter reference is a resource, just not this one. The type check has
%% to turn that into an error rather than a badarg or a dereference.
add_delegate_rejects_a_non_delegate(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    ?assertMatch({error, _}, tflite_beam_interpreter_builder:add_delegate(Builder, Interpreter)).

builder_survives_a_rejected_delegate(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    {error, _} = tflite_beam_interpreter_builder:add_delegate(Builder, Interpreter),
    ?assertEqual(ok, tflite_beam_interpreter_builder:build(Builder, Interpreter)),
    ?assertEqual(ok, tflite_beam_interpreter:allocate_tensors(Interpreter)),
    ?assertEqual(7, tflite_beam_interpreter:tensors_size(Interpreter)).

%% Pins that add_delegate/2 is add_delegate/3 with the default policy. Until a
%% constructor exists all three can only be compared on the rejection path,
%% which shows they take the same route but not what the policy then does.
on_decline_default_is_error(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    Implicit = tflite_beam_interpreter_builder:add_delegate(Builder, Interpreter),
    Empty = tflite_beam_interpreter_builder:add_delegate(Builder, Interpreter, #{}),
    Explicit = tflite_beam_interpreter_builder:add_delegate(Builder, Interpreter, #{on_decline => error}),
    ?assertMatch({error, _}, Implicit),
    ?assertEqual(Implicit, Empty),
    ?assertEqual(Empty, Explicit).

%% fallback has to get past validation and be refused by the resource check,
%% not by the option check -- otherwise the policy would never reach the NIF.
on_decline_accepts_fallback(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    Fallback = tflite_beam_interpreter_builder:add_delegate(Builder, Interpreter, #{on_decline => fallback}),
    Default = tflite_beam_interpreter_builder:add_delegate(Builder, Interpreter),
    ?assertEqual(Default, Fallback).

%% Rejected at the Erlang boundary, before any NIF call: the message is about
%% the option, not about the resource that would have failed next.
on_decline_rejects_unknown_value(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    {error, Reason} = tflite_beam_interpreter_builder:add_delegate(Builder, Interpreter, #{on_decline => 'maybe'}),
    ?assertNotEqual(nomatch, binary:match(Reason, <<"on_decline">>)).

unknown_option_is_rejected(_Config) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    {error, Reason} = tflite_beam_interpreter_builder:add_delegate(Builder, Interpreter, #{on_declines => fallback}),
    ?assertNotEqual(nomatch, binary:match(Reason, <<"on_declines">>)).

%% Every case below needs a delegate to exist. XNNPACK is compiled into every
%% target except armv6 and armv7l, and available/0 is what says so -- not the
%% architecture string.
xnnpack_is_compiled_in(_Config) ->
    case has_xnnpack() of
        true ->
            ?assert(lists:member(xnnpack, tflite_beam_delegate:available())),
            ?assertMatch({ok, _}, tflite_beam_delegate:xnnpack());
        false ->
            %% where it is not, asking has to be an error and never a crash
            ?assertMatch({error, _}, tflite_beam_delegate:xnnpack()),
            {skip, "XNNPACK is not compiled into this build"}
    end.

%% Flags are mapped by name because bit 0x100 is unassigned, so nothing
%% positional could be right.
xnnpack_rejects_unknown_flag_and_option(_Config) ->
    skip_without_xnnpack(fun() ->
        ?assertMatch({ok, _}, tflite_beam_delegate:xnnpack(#{flags => [qs8, force_fp16]})),
        {error, Flag} = tflite_beam_delegate:xnnpack(#{flags => [not_a_flag]}),
        ?assertNotEqual(nomatch, binary:match(Flag, <<"not_a_flag">>)),
        {error, Option} = tflite_beam_delegate:xnnpack(#{threads => 2}),
        ?assertNotEqual(nomatch, binary:match(Option, <<"threads">>))
    end).

%% The discriminating one: an explicitly attached delegate claims the graph
%% during build/2, where the lazy one only ever claims it at allocate time.
xnnpack_explicit_applies_at_build(_Config) ->
    skip_without_xnnpack(fun() ->
        {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
        {ok, Delegate} = tflite_beam_delegate:xnnpack(),
        ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),
        ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
        ?assertEqual(1, length(tflite_beam_interpreter:execution_plan(Interpreter))),
        ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
        ?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                     tflite_beam_interpreter:predict(Interpreter, ?INPUTS))
    end).

%% Nothing delegates when the resolver declines TfLite's own delegates and no
%% delegate is attached -- which is what makes the Phase 01 baseline meaningful.
%% Built through the NIF on purpose: build/2 would otherwise attach the default
%% XNNPACK, and this case is about the layer underneath that.
default_delegates_opt_out(_Config) ->
    skip_without_xnnpack(fun() ->
        {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
        ok = tflite_beam_nif:interpreter_builder_build(Builder, Interpreter),
        ?assertEqual([0, 1, 2], tflite_beam_interpreter:execution_plan(Interpreter)),
        ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
        ?assertEqual([0, 1, 2], tflite_beam_interpreter:execution_plan(Interpreter)),
        ?assertEqual(3, tflite_beam_interpreter:nodes_size(Interpreter))
    end).

%% The flip's acceptance criterion. Delegating explicitly at build time has to
%% land in the same place as TfLite delegating lazily at allocate time: same
%% output, same node count, same execution plan.
default_path_matches_old_default(_Config) ->
    skip_without_xnnpack(fun() ->
        [begin
            New = built(Model, #{}),
            Old = built(Model, #{apply_default_delegates => true}),
            ?assertEqual(Old, New)
         end || Model <- ["multi_add.bin", "add.bin"]]
    end).

%% set_num_threads/2 drives TfLite's CPU backend, which is where the lazy
%% delegate took its thread count from. The explicit one carries its own pool, so
%% the builder's value has to be handed to it or the setting silently stops
%% reaching XNNPACK. The count itself is not observable through anything this
%% library exposes; what is checkable is that the builder carries it, and that
%% exactly one delegate ends up attached.
set_num_threads_reaches_the_default_delegate(_Config) ->
    skip_without_xnnpack(fun() ->
        {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
        ?assertEqual({ok, {0, -1, false}}, tflite_beam_nif:interpreter_builder_state(Builder)),
        ok = tflite_beam_interpreter_builder:set_num_threads(Builder, 4),
        ?assertEqual({ok, {0, 4, false}}, tflite_beam_nif:interpreter_builder_state(Builder)),
        ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
        ?assertEqual({ok, {1, 4, false}}, tflite_beam_nif:interpreter_builder_state(Builder)),
        ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
        ?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                     tflite_beam_interpreter:predict(Interpreter, ?INPUTS))
    end).

xnnpack_num_threads(_Config) ->
    skip_without_xnnpack(fun() ->
        ?assertEqual(delegated_output(#{num_threads => 1}), delegated_output(#{num_threads => 4}))
    end).

xnnpack_weight_cache_file_path(Config) ->
    skip_without_xnnpack(fun() ->
        Path = filename:join(?config(priv_dir, Config), "xnnpack.cache"),
        ?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                     delegated_output(#{weight_cache_file_path => Path}))
    end).

%% The delegate has to outlive every interpreter built from the builder, and
%% neither the builder's term nor the delegate's is what keeps it alive.
delegate_outlives_builder(_Config) ->
    skip_without_xnnpack(fun() ->
        Interpreter = interpreter_from_a_dropped_builder(),
        erlang:garbage_collect(),
        timer:sleep(100),
        ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
        ?assertEqual(1, length(tflite_beam_interpreter:execution_plan(Interpreter))),
        ?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                     tflite_beam_interpreter:predict(Interpreter, ?INPUTS))
    end).

%% One delegate behind two interpreters, sequentially -- TfLite supports reusing
%% a builder across successive builds. Concurrently is a different question and
%% the answer is no; see the threading note in the README.
delegate_two_interpreters(_Config) ->
    skip_without_xnnpack(fun() ->
        {Builder, First} = tflite_beam_test_models:builder("multi_add.bin"),
        {ok, Delegate} = tflite_beam_delegate:xnnpack(),
        ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),
        {ok, Second} = tflite_beam_interpreter:new(),
        ok = tflite_beam_interpreter_builder:build(Builder, First),
        ok = tflite_beam_interpreter_builder:build(Builder, Second),
        [ok = tflite_beam_interpreter:allocate_tensors(I) || I <- [First, Second]],
        [?assertEqual([?FILLED(6.0), ?FILLED(9.0)],
                      tflite_beam_interpreter:predict(I, ?INPUTS)) || I <- [First, Second]]
    end).

interpreter_from_a_dropped_builder() ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    {ok, Delegate} = tflite_beam_delegate:xnnpack(),
    ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    Interpreter.

delegated_output(Opts) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("multi_add.bin"),
    {ok, Delegate} = tflite_beam_delegate:xnnpack(Opts),
    ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    tflite_beam_interpreter:predict(Interpreter, ?INPUTS).

built(Model, ResolverOpts) ->
    {ok, Resolver} = tflite_beam_ops_builtin_builtin_resolver:new(ResolverOpts),
    Loaded = tflite_beam_flatbuffer_model:build_from_file(tflite_beam_test_models:path(Model)),
    {ok, Builder} = tflite_beam_interpreter_builder:new(element(4, Loaded), Resolver),
    {ok, Interpreter} = tflite_beam_interpreter:new(),
    ok = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    ok = tflite_beam_interpreter:allocate_tensors(Interpreter),
    {tflite_beam_interpreter:execution_plan(Interpreter),
     tflite_beam_interpreter:nodes_size(Interpreter),
     tflite_beam_interpreter:predict(Interpreter, inputs_for(Model))}.

inputs_for("multi_add.bin") -> ?INPUTS;
inputs_for(_) -> [?FILLED(1.0)].

has_xnnpack() ->
    lists:member(xnnpack, tflite_beam_delegate:available()).

skip_without_xnnpack(Body) ->
    case has_xnnpack() of
        true -> Body();
        false -> {skip, "XNNPACK is not compiled into this build"}
    end.

%% A delegate that cannot take the graph but leaves it runnable reports
%% kTfLiteApplicationError, and TfLite discards the interpreter anyway. XNNPACK
%% does exactly that on a model with dynamic shapes once subgraph reshaping is
%% turned off -- with it on, which is the default, the same model delegates
%% fine, so the flag is what causes this and not the model.
declining_delegate_is_an_error_by_default(_Config) ->
    skip_without_xnnpack(fun() ->
        ?assertMatch({ok, ok, 28}, dynamic_shapes([])),
        {Built, _, Nodes} = dynamic_shapes([disable_subgraph_reshaping]),
        ?assertMatch({error, _}, Built),
        ?assertMatch({error, _}, Nodes)
    end).

%% And with fallback: built again without the delegate that stepped aside, which
%% leaves the plain CPU graph -- 24 nodes rather than the 28 a delegated one has.
declining_delegate_falls_back(_Config) ->
    skip_without_xnnpack(fun() ->
        ?assertMatch({{ok, delegate_declined}, ok, 24},
                     dynamic_shapes([disable_subgraph_reshaping], fallback))
    end).

%% fallback covers a decline and nothing else. A model that cannot be built at
%% all still fails, with no interpreter left behind.
fallback_is_not_a_blanket_catch(_Config) ->
    skip_without_xnnpack(fun() ->
        {Builder, Interpreter} = tflite_beam_test_models:builder("0_subgraphs.bin"),
        {ok, Delegate} = tflite_beam_delegate:xnnpack(),
        ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate, #{on_decline => fallback}),
        ?assertMatch({error, _}, tflite_beam_interpreter_builder:build(Builder, Interpreter)),
        ?assertMatch({error, _}, tflite_beam_interpreter:nodes_size(Interpreter))
    end).

dynamic_shapes(Flags) ->
    dynamic_shapes(Flags, error).

dynamic_shapes(Flags, OnDecline) ->
    {Builder, Interpreter} = tflite_beam_test_models:builder("dynamic_shapes.bin"),
    {ok, Delegate} = tflite_beam_delegate:xnnpack(#{flags => Flags}),
    ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate, #{on_decline => OnDecline}),
    Built = tflite_beam_interpreter_builder:build(Builder, Interpreter),
    Allocated = case Built of
        {error, _} -> not_attempted;
        _ -> tflite_beam_interpreter:allocate_tensors(Interpreter)
    end,
    {Built, Allocated, tflite_beam_interpreter:nodes_size(Interpreter)}.
