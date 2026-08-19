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
    unknown_option_is_rejected/1
]).

all() ->
    [
        available_is_a_list,
        add_delegate_rejects_a_non_delegate,
        builder_survives_a_rejected_delegate,
        on_decline_default_is_error,
        on_decline_accepts_fallback,
        on_decline_rejects_unknown_value,
        unknown_option_is_rejected
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
