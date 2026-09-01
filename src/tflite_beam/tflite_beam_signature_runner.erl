%% @doc A runner for one of a model's signatures.
%%
%% A signature names a subgraph together with its inputs and outputs, so the tensors
%% are addressed by name rather than by index and the ordering of a model's outputs no
%% longer has to be guessed.
%%
%% A runner belongs to the interpreter it came from and cannot outlive it. Like the
%% interpreter itself it is not safe to use from more than one process at a time;
%% serialise access if several processes share one.
-module(tflite_beam_signature_runner).
-export([
    signature_key/1,
    input_size/1,
    output_size/1,
    input_names/1,
    output_names/1,
    input_tensor/3,
    output_tensor/2,
    resize_input_tensor/3,
    resize_input_tensor_strict/3,
    allocate_tensors/1,
    invoke/1,
    cancel/1
]).

%% @doc The key this runner was obtained with.
-spec signature_key(reference()) -> {ok, binary()} | {error, binary()}.
signature_key(Self) when is_reference(Self) ->
    tflite_beam_nif:signature_runner_signature_key(Self).

%% @doc How many inputs the signature has.
-spec input_size(reference()) -> {ok, non_neg_integer()} | {error, binary()}.
input_size(Self) when is_reference(Self) ->
    tflite_beam_nif:signature_runner_input_size(Self).

%% @doc How many outputs the signature has.
-spec output_size(reference()) -> {ok, non_neg_integer()} | {error, binary()}.
output_size(Self) when is_reference(Self) ->
    tflite_beam_nif:signature_runner_output_size(Self).

%% @doc The names of the signature's inputs.
-spec input_names(reference()) -> {ok, list(binary())} | {error, binary()}.
input_names(Self) when is_reference(Self) ->
    tflite_beam_nif:signature_runner_input_names(Self).

%% @doc The names of the signature's outputs.
-spec output_names(reference()) -> {ok, list(binary())} | {error, binary()}.
output_names(Self) when is_reference(Self) ->
    tflite_beam_nif:signature_runner_output_names(Self).

%% @doc Write data into the named input.
%%
%% `allocate_tensors/1' has to have been called first.
-spec input_tensor(reference(), binary() | list(), binary()) -> ok | {error, binary()}.
input_tensor(Self, InputName, Data) when is_reference(Self), is_list(InputName) ->
    input_tensor(Self, unicode:characters_to_binary(InputName), Data);
input_tensor(Self, InputName, Data) when is_reference(Self), is_binary(InputName), is_binary(Data) ->
    tflite_beam_nif:signature_runner_input_tensor(Self, InputName, Data).

%% @doc Read the named output.
-spec output_tensor(reference(), binary() | list()) -> {ok, binary()} | {error, binary()}.
output_tensor(Self, OutputName) when is_reference(Self), is_list(OutputName) ->
    output_tensor(Self, unicode:characters_to_binary(OutputName));
output_tensor(Self, OutputName) when is_reference(Self), is_binary(OutputName) ->
    tflite_beam_nif:signature_runner_output_tensor(Self, OutputName).

%% @doc Change the dimensions of the named input.
%%
%% `allocate_tensors/1' has to be called again afterwards.
-spec resize_input_tensor(reference(), binary() | list(), list(integer())) -> ok | {error, binary()}.
resize_input_tensor(Self, InputName, Dims) when is_reference(Self), is_list(InputName), is_list(Dims) ->
    resize_input_tensor(Self, unicode:characters_to_binary(InputName), Dims);
resize_input_tensor(Self, InputName, Dims) when is_reference(Self), is_binary(InputName), is_list(Dims) ->
    tflite_beam_nif:signature_runner_resize_input_tensor(Self, InputName, Dims).

%% @doc Change the dimensions of the named input, keeping the rank fixed.
%%
%% Only dimensions the model left unknown can be changed.
-spec resize_input_tensor_strict(reference(), binary() | list(), list(integer())) -> ok | {error, binary()}.
resize_input_tensor_strict(Self, InputName, Dims) when is_reference(Self), is_list(InputName), is_list(Dims) ->
    resize_input_tensor_strict(Self, unicode:characters_to_binary(InputName), Dims);
resize_input_tensor_strict(Self, InputName, Dims) when is_reference(Self), is_binary(InputName), is_list(Dims) ->
    tflite_beam_nif:signature_runner_resize_input_tensor_strict(Self, InputName, Dims).

%% @doc Allocate the tensors of the signature's subgraph.
-spec allocate_tensors(reference()) -> ok | {error, binary()}.
allocate_tensors(Self) when is_reference(Self) ->
    tflite_beam_nif:signature_runner_allocate_tensors(Self).

%% @doc Run the signature.
-spec invoke(reference()) -> ok | {error, binary()}.
invoke(Self) when is_reference(Self) ->
    tflite_beam_nif:signature_runner_invoke(Self).

%% @doc Cancel an in-flight invocation.
%%
%% Requires `tflite_beam_interpreter:enable_cancellation/1' on the interpreter
%% this runner came from. Without it TFLite refuses, and the reason it gives
%% does not say why.
-spec cancel(reference()) -> ok | {error, binary()}.
cancel(Self) when is_reference(Self) ->
    tflite_beam_nif:signature_runner_cancel(Self).
