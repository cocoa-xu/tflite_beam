%% @doc
%% Build an interpreter capable of interpreting model.

-module(tflite_beam_interpreter_builder).
-export([
    new/2,
    build/2,
    set_num_threads/2
]).

-include("tflite_beam_records.hrl").

%% @doc New InterpreterBuilder
-spec new(#tflite_beam_flatbuffer_model{} | reference(), reference()) -> {ok, reference()} | {error, binary()}.
new(#tflite_beam_flatbuffer_model{ref = Model}, Resolver) when is_reference(Model) and is_reference(Resolver) ->
    new(Model, Resolver);
new(Model, Resolver) when is_reference(Model) and is_reference(Resolver) ->
    tflite_beam_nif:interpreter_builder_new(Model, Resolver).

%% @doc
%% Build the interpreter with the InterpreterBuilder.
%%
%% Note: all Interpreters should be built with the InterpreterBuilder,
%% which allocates memory for the Interpreter and does various set up
%% tasks so that the Interpreter can read the provided model.
-spec build(reference(), reference()) -> ok | {error, binary}.
build(Builder, Interpreter) when is_reference(Builder) and is_reference(Interpreter) ->
    tflite_beam_nif:interpreter_builder_build(Builder, Interpreter).

%% @doc
%% Sets the number of CPU threads to use for the interpreter.
%% Returns `true' on success, `{error, reason}' on error.
-spec set_num_threads(reference(), pos_integer()) -> ok | {error, binary}.
set_num_threads(Builder, NumThreads) when is_reference(Builder) and is_integer(NumThreads) ->
    tflite_beam_nif:interpreter_builder_set_num_threads(Builder, NumThreads).
