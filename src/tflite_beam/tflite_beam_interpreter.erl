%% @moduledoc
%% An interpreter for a graph of nodes that input and output from tensors.

-module(tflite_beam_interpreter).
-export([
   new/0,
   new/1,
   new_from_buffer/1,
   set_inputs/2,
   set_outputs/2,
   set_variables/2,
   inputs/1,
   get_input_name/2,
   outputs/1,
   variables/1,
   get_output_name/2,
   tensors_size/1,
   nodes_size/1,
   execution_plan/1,
   tensor/2,
   signature_keys/1,
   input_tensor/3,
   output_tensor/2,
   allocate_tensors/1,
   invoke/1,
   set_num_threads/2,
   get_signature_defs/1,
   predict/2
]).

-include("tflite_beam_records.hrl").

%% @doc New interpreter
-spec new() -> {ok, reference()} | {error, binary()}.
new() ->
    tflite_beam_nif:interpreter_new().

%% @doc New interpreter with model filepath
-spec new(list() | binary()) -> {ok, reference()} | {error, binary()}.
new(ModelPath) when is_list(ModelPath) ->
    new(unicode:characters_to_binary(ModelPath));
new(ModelPath) when is_binary(ModelPath) ->
    case tflite_beam_flatbuffer_model:build_from_file(ModelPath) of
        #tflite_beam_flatbuffer_model{ref = Model} ->
            new_from_model(Model);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc New interpreter with model buffer
-spec new_from_buffer(binary()) -> {ok, reference()} | {error, binary()}.
new_from_buffer(Buffer) ->
    case tflite_beam_flatbuffer_model:build_from_buffer(Buffer) of
        #tflite_beam_flatbuffer_model{ref = Model} ->
            new_from_model(Model);
        {error, Reason} ->
            {error, Reason}
    end.

new_from_model(Model) when is_reference(Model) ->
    case tflite_beam_ops_builtin_builtin_resolver:new() of
        {ok, Resolver} ->
            case tflite_beam_interpreter_builder:new(Model, Resolver) of
                {ok, Builder} ->
                    case tflite_beam_interpreter:new() of
                        {ok, Interpreter} ->
                            case tflite_beam_interpreter_builder:build(Builder, Interpreter) of
                                ok ->
                                    case tflite_beam_interpreter:allocate_tensors(Interpreter) of
                                        ok ->
                                            {ok, Interpreter};
                                        {error, Reason} ->
                                            {error, Reason}
                                    end;
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc
%% Provide a list of tensor indexes that are inputs to the model.
%% Each index is bound check and this modifies the consistent_ flag of the
%% interpreter.
-spec set_inputs(reference(), list(integer())) -> ok | {error, binary()}.
set_inputs(Self, Inputs) when is_reference(Self) and is_list(Inputs) ->
    tflite_beam_nif:interpreter_set_inputs(Self, Inputs).

%% @doc
%% Provide a list of tensor indexes that are outputs to the model.
%% Each index is bound check and this modifies the consistent_ flag of the
%% interpreter.
-spec set_outputs(reference(), list(integer())) -> ok | {error, binary()}.
set_outputs(Self, Outputs) when is_reference(Self) and is_list(Outputs) ->
    tflite_beam_nif:interpreter_set_outputs(Self, Outputs).

%% @doc
%% Provide a list of tensor indexes that are variable tensors.
%% Each index is bound check and this modifies the consistent_ flag of the
%% interpreter.
-spec set_variables(reference(), list(integer())) -> ok | {error, binary()}.
set_variables(Self, Variables) when is_reference(Self) and is_list(Variables) ->
    tflite_beam_nif:interpreter_set_variables(Self, Variables).

%% @doc
%% Get the list of input tensors.
%% 
%% return a list of input tensor id
-spec inputs(reference()) -> {ok, [non_neg_integer()]} | {error, binary()}.
inputs(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_inputs(Self).

%% @doc
%% Get the name of the input tensor
%% 
%% Note that the index here means the index in the result list of `inputs/1'. For example,
%% if `inputs/1' returns `[42, 314]', then `0' should be passed here to get the name of
%% tensor `42'
-spec get_input_name(reference(), non_neg_integer()) -> {ok, binary()} | {error, binary()}.
get_input_name(Self, Index) when is_reference(Self) and is_integer(Index) ->
    tflite_beam_nif:interpreter_get_input_name(Self, Index).

%% @doc
%% Get the list of output tensors.
%% 
%% return a list of output tensor id
-spec outputs(reference()) -> {ok, list(non_neg_integer())} | {error, binary()}.
outputs(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_outputs(Self).

%% @doc Get the list of variable tensors.
-spec variables(reference()) -> {ok, list(non_neg_integer())} | {error, binary()}.
variables(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_variables(Self).

%% @doc
%% Get the name of the output tensor
%% 
%% Note that the index here means the index in the result list of `outputs/1'. For example,
%% if `outputs/1' returns `[42, 314]', then `0' should be passed here to get the name of
%% tensor `42'
-spec get_output_name(reference(), non_neg_integer()) -> {ok, binary()} | {error, binary()}.
get_output_name(Self, Index) when is_reference(Self) and is_integer(Index) ->
    tflite_beam_nif:interpreter_get_output_name(Self, Index).

%% @doc Return the number of tensors in the model.
-spec tensors_size(reference()) -> non_neg_integer() | {error, binary()}.
tensors_size(Self) when is_reference(Self) ->
    case tflite_beam_nif:interpreter_tensors_size(Self) of
        {ok, TensorSize} -> 
            TensorSize;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Return the number of ops in the model.
-spec nodes_size(reference()) -> non_neg_integer() | {error, binary()}.
nodes_size(Self) when is_reference(Self) ->
    case tflite_beam_nif:interpreter_nodes_size(Self) of
        {ok, NodesSize} -> 
            NodesSize;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc
%% Return the execution plan of the model.
%%
%% Experimental interface, subject to change.
-spec execution_plan(reference()) -> list(non_neg_integer()) | {error, binary()}.
execution_plan(Self) when is_reference(Self) ->
    case tflite_beam_nif:interpreter_execution_plan(Self) of
        {ok, ExecutionPlan} -> 
            ExecutionPlan;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc
%% Get any tensor in the graph by its id
%%
%% Note that the `tensor_index' here means the id of a tensor. For example,
%% if `inputs/1' returns `[42, 314]', then `42' should be passed here to get tensor `42'.
-spec tensor(reference(), non_neg_integer()) -> #tflite_beam_tensor{} | {error, binary()}.
tensor(Self, TensorIndex) when is_reference(Self) and is_integer(TensorIndex) ->
    case tflite_beam_nif:interpreter_tensor(Self, TensorIndex) of
        {ok, {Name, Index, Shape, ShapeSignature, Type, {Scale, ZeroPoint, QuantizedDimension}, SparsityParams, Ref}} ->
            #tflite_beam_tensor{
                name = Name, 
                index = Index,
                shape = Shape,
                shape_signature = ShapeSignature,
                type = Type,
                quantization_params = #tflite_beam_quantization_params{
                    scale = Scale,
                    zero_point = ZeroPoint,
                    quantized_dimension = QuantizedDimension
                },
                sparsity_params = SparsityParams,
                ref = Ref
            };
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc
%% Returns list of all keys of different method signatures defined in the
%% model.
%%
%% WARNING: Experimental interface, subject to change
-spec signature_keys(reference()) -> list(binary()) | {error, binary()}.
signature_keys(Self) when is_reference(Self) ->
    case tflite_beam_nif:interpreter_signature_keys(Self) of
        {ok, SignatureKeys} ->
            SignatureKeys;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc
%% Fill data to the specified input tensor
%%
%% Note: although we have `typed_input_tensor' available in C++, here what we really passed
%% to the NIF is `binary` data, therefore, I'm not pretend that we have type information.
-spec input_tensor(reference(), non_neg_integer(), binary()) -> ok | {error, binary()}.
input_tensor(Self, Index, Data) when is_reference(Self) and is_integer(Index) and is_binary(Data) ->
    tflite_beam_nif:interpreter_input_tensor(Self, Index, Data).

%% @doc
%% Get the data of the output tensor
%%
%% Note that the index here means the index in the result list of `outputs/1'. For example,
%% if `outputs/1' returns `[42, 314]', then `0` should be passed here to get the name of
%% tensor `42'
-spec output_tensor(reference(), non_neg_integer()) -> {ok, binary()} | {error, binary()}.
output_tensor(Self, Index) when is_reference(Self) and is_integer(Index) ->
    tflite_beam_nif:interpreter_output_tensor(Self, Index).

%% @doc Allocate memory for tensors in the graph
-spec allocate_tensors(reference()) -> ok | {error, binary()}.
allocate_tensors(Self) ->
    tflite_beam_nif:interpreter_allocate_tensors(Self).

%% @doc Run forwarding
-spec invoke(reference()) -> ok | {error, binary()}.
invoke(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_invoke(Self).

%% @doc
%% Set the number of threads available to the interpreter.
%%
%% As TfLite interpreter could internally apply a TfLite delegate by default
%% (i.e. XNNPACK), the number of threads that are available to the default
%% delegate should be set via InterpreterBuilder APIs as follows:
%%
%% ```
%% {ok, Interpreter} = tflite_beam_interpreter:new(),
%% {ok, Builder} = tflite_beam_interpreter_builder:new(Model, Resolver),
%% tflite_beam_interpreter_builder:set_num_threads(Builder, NumThreads),
%% tflite_beam_interpreter_builder:build(Builder, Interpreter)
%% '''
-spec set_num_threads(reference(), integer()) -> ok | {error, binary()}.
set_num_threads(Self, NumThreads) when is_reference(Self) and is_integer(NumThreads) ->
    tflite_beam_nif:interpreter_set_num_threads(Self, NumThreads).

%% @doc
%% Get SignatureDef map from the Metadata of a TfLite FlatBuffer buffer.
%%
%% @return A map containing serving names to SignatureDefs if exists, otherwise, `nil'.
-spec get_signature_defs(reference()) -> {ok, map()} | nil | {error, binary()}.
get_signature_defs(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_get_signature_defs(Self).

%% @doc
%% Fill input data to corresponding input tensor of the interpreter,
%% call `tflite_beam_interpreter:invoke/1' and return output tensor(s).
-spec predict(reference(), list(binary()) | binary() | map()) -> list(#tflite_beam_tensor{} | {error, binary()}) | #tflite_beam_tensor{} | {error, binary()}.
predict(Self, Input) when is_reference(Self) and (is_binary(Input) or is_list(Input) or is_map(Input)) ->
    case tflite_beam_interpreter:inputs(Self) of
        {ok, InputTensors} ->
            case tflite_beam_interpreter:outputs(Self) of
                {ok, OutputTensors} ->
                    case fill_input(Self, InputTensors, Input) of
                        ok ->
                            tflite_beam_interpreter:invoke(Self),
                            fetch_output(Self, OutputTensors);
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

fill_input(Self, InputTensors, Input) when is_reference(Self) and is_list(InputTensors) and is_binary(Input) ->
    fill_input(Self, InputTensors, [Input]);
fill_input(Self, InputTensors, Input) when is_reference(Self) and is_list(InputTensors) and is_list(Input) ->
    if length(InputTensors) == length(Input) ->
        FillResults = lists:zipwith(
            fun(InputTensorIndex, InputData) ->
                fill_input(Self, InputTensorIndex, InputData)
            end,
            InputTensors,
            Input
        ),
        AllFilled = lists:all(
            fun(R) ->
                R == ok
            end,
            FillResults
        ),
        if 
            AllFilled ->
                ok;
            true ->
                not_ok_to_reason(FillResults)
        end;
    true ->
        Reason = io_lib:format("length mismatch: there are ~w input tensors while the input list has ~w elements", [length(InputTensors), length(Input)]),
        {error, unicode:characters_to_binary(Reason)}
    end;
fill_input(Self, InputTensorIndex, InputData) when is_reference(Self) and is_integer(InputTensorIndex) and is_binary(InputData) ->
    case tflite_beam_interpreter:tensor(Self, InputTensorIndex) of
        #tflite_beam_tensor{} = Tensor ->
            tflite_beam_tensor:set_data(Tensor, InputData);
        {error, Reason} ->
            {error, Reason}
    end;
fill_input(Self, InputTensors, InputMap) when is_reference(Self) and is_list(InputTensors) and is_map(InputMap) ->
    FillResults = lists:map(
        fun(InputTensorIndex) ->
            case tflite_beam_interpreter:tensor(Self, InputTensorIndex) of
                #tflite_beam_tensor{name = Name} = Tensor ->
                    HasInput = maps:is_key(Name, InputMap),
                    if 
                        HasInput ->
                            InputData = maps:get(Name, InputMap),
                            tflite_beam_tensor:set_data(Tensor, InputData);
                        true ->
                            Reason = io_lib:format("missing input data for tensor `~ts`, tensor index: ~w", [Name, InputTensorIndex]),
                            unicode:characters_to_binary(Reason)
                    end;
                {error, Reason} ->
                    Reason
            end
        end,
        InputTensors
    ),
    not_ok_to_reason(FillResults).

fetch_output(Self, OutputTensors) when is_reference(Self) and is_list(OutputTensors) ->
    lists:map(
        fun(OutputTensorIndex) ->
            fetch_output(Self, OutputTensorIndex)
        end,
        OutputTensors
    );
fetch_output(Self, OutputTensorIndex) when is_reference(Self) and is_integer(OutputTensorIndex) ->
    case tflite_beam_interpreter:tensor(Self, OutputTensorIndex) of
        #tflite_beam_tensor{} = Tensor ->
            tflite_beam_tensor:to_binary(Tensor);
        {error, Reason} ->
            {error, Reason}
    end.

not_ok_to_reason(Results) when is_list(Results) ->
    Filtered = lists:filter(
        fun(R) ->
            not (R == ok)
        end,
        Results
    ),
    case Filtered of
        [] -> 
            ok;
        _ ->
            Reason = lists:foldl(fun(R, Acc) -> <<Acc/binary, <<"; ">>/binary, R/binary>> end, <<"">>, Filtered),
            {error, binary:part(Reason, {2, byte_size(Reason) - 2})}
    end.
