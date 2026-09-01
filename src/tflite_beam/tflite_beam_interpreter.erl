%% @doc
%% An interpreter for a graph of nodes that input and output from tensors.

-module(tflite_beam_interpreter).
-export([
   new/0,
   new/1,
   controlling_process/1, controlling_process/2,
   new_from_buffer/1,
   set_inputs/2,
   set_outputs/2,
   set_variables/2,
   resize_input_tensor/3,
   resize_input_tensor_strict/3,
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
   get_signature_runner/2,
   signature_inputs/2,
   signature_outputs/2,
   get_subgraph_index_from_signature/2,
   enable_cancellation/1,
   cancel/1,
   release_non_persistent_memory/1,
   reset_variable_tensors/1,
   subgraphs_size/1,
   get_allow_fp16_precision_for_fp32/1,
   set_allow_fp16_precision_for_fp32/2,
   predict/2
]).

-include("tflite_beam_records.hrl").

%% @doc New interpreter
-spec new() -> {ok, reference()} | {error, binary()}.
new() ->
    tflite_beam_nif:interpreter_new().

%% @doc
%% Which process this interpreter belongs to, or `undefined' if it is shared.
-spec controlling_process(reference()) -> {ok, pid()} | undefined.
controlling_process(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_controlling_process(Self).

%% @doc
%% Give this interpreter to a process, after which no other process may use it.
%%
%% `tflite::Interpreter' is not thread-safe and `invoke/1' runs on a dirty
%% scheduler, so two processes sharing one interpreter really do reach it on two
%% OS threads. Measured on a real model, two processes taking turns badly got the
%% wrong inference back 147 times out of 400 -- not a crash, just quietly
%% somebody else's answer.
%%
%% An interpreter starts out belonging to nobody, which is how they have always
%% behaved, and calls from concurrent processes are refused only while they
%% actually overlap. Naming a controlling process closes the remaining window:
%% every other process is then refused outright, whether it overlaps or not.
%%
%% Follows `gen_tcp:controlling_process/2': while an interpreter belongs to
%% nobody any process may take it, and once it belongs to someone only that
%% process may hand it on. Pass `undefined' to give it back to nobody. A
%% controlling process that dies releases it, since an interpreter has no
%% equivalent of a socket being closed.
-spec controlling_process(reference(), pid() | undefined) -> ok | {error, binary()}.
controlling_process(Self, Pid) when is_reference(Self), is_pid(Pid) orelse Pid =:= undefined ->
    tflite_beam_nif:interpreter_set_controlling_process(Self, Pid).

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
                                Built when Built =:= ok; Built =:= {ok, delegate_declined} ->
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
%% Change the dimensionality of a given input tensor.
%%
%% Note that this is only acceptable for tensors that are inputs to the model,
%% and `allocate_tensors/1' has to be called again afterwards.
%%
%% `Dims' is a list, or the tuple `tflite_beam_tensor:shape/1' returns.
-spec resize_input_tensor(reference(), integer(), list(integer()) | tuple()) -> ok | {error, binary()}.
resize_input_tensor(Self, TensorIndex, Dims) when is_tuple(Dims) ->
    resize_input_tensor(Self, TensorIndex, tuple_to_list(Dims));
resize_input_tensor(Self, TensorIndex, Dims) when is_reference(Self), is_integer(TensorIndex), is_list(Dims) ->
    tflite_beam_nif:interpreter_resize_input_tensor(Self, TensorIndex, Dims).

%% @doc
%% Change the dimensionality of a given input tensor, keeping the rank fixed.
%%
%% Unlike `resize_input_tensor/3', this only accepts dimensions that the model
%% left unknown, so a tensor whose shape is fully fixed cannot be resized.
%%
%% `Dims' is a list, or the tuple `tflite_beam_tensor:shape/1' returns.
-spec resize_input_tensor_strict(reference(), integer(), list(integer()) | tuple()) -> ok | {error, binary()}.
resize_input_tensor_strict(Self, TensorIndex, Dims) when is_tuple(Dims) ->
    resize_input_tensor_strict(Self, TensorIndex, tuple_to_list(Dims));
resize_input_tensor_strict(Self, TensorIndex, Dims) when is_reference(Self), is_integer(TensorIndex), is_list(Dims) ->
    tflite_beam_nif:interpreter_resize_input_tensor_strict(Self, TensorIndex, Dims).

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
%%
%% The handle borrows the interpreter's memory and keeps the interpreter alive
%% for as long as the handle itself is reachable, so there is nothing a caller has
%% to hold on its behalf. What the handle cannot survive is the memory moving:
%% `allocate_tensors/1', either `resize_input_tensor' and a second
%% `tflite_beam_interpreter_builder:build/2' all relocate what it points at, and
%% reading through it afterwards returns `{error, Reason}'. Fetch it again after
%% any of those.
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
%%
%% `NumThreads' follows TfLite: `-1' asks the runtime to choose, `0' means the
%% same as `1', and anything below `-1' is refused.
-spec set_num_threads(reference(), integer()) -> ok | {error, binary()}.
set_num_threads(Self, NumThreads) when is_reference(Self) and is_integer(NumThreads) ->
    tflite_beam_nif:interpreter_set_num_threads(Self, NumThreads).

%% @doc
%% Get SignatureDef map from the Metadata of a TfLite FlatBuffer buffer.
%%
%% @return `{ok, Map}' of serving names to SignatureDefs, or `{ok, nil}' for a
%% model that declares none. The bare `nil' this used to promise is not a shape
%% the function can produce, so anyone matching on it never matched.
-spec get_signature_defs(reference()) -> {ok, map() | nil} | {error, binary()}.
get_signature_defs(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_get_signature_defs(Self).

%% @doc
%% Get a runner for one of the model's signatures.
%%
%% The runner addresses its tensors by name and belongs to this interpreter: it must
%% not be used once the interpreter is gone. Passing a key the model does not declare
%% is an error. Pass `nil' for the primary subgraph: the first signature that points
%% at it, or a placeholder one when the model declares no signatures at all.
%% See `tflite_beam_signature_runner'.
-spec get_signature_runner(reference(), binary() | list() | nil) -> {ok, reference()} | {error, binary()}.
get_signature_runner(Self, nil) when is_reference(Self) ->
    tflite_beam_nif:interpreter_get_signature_runner(Self, nil);
get_signature_runner(Self, SignatureKey) when is_reference(Self), is_list(SignatureKey) ->
    get_signature_runner(Self, unicode:characters_to_binary(SignatureKey));
get_signature_runner(Self, SignatureKey) when is_reference(Self), is_binary(SignatureKey) ->
    tflite_beam_nif:interpreter_get_signature_runner(Self, SignatureKey).

%% @doc
%% Fill input data to corresponding input tensor of the interpreter,
%% call `tflite_beam_interpreter:invoke/1' and return output tensor(s).
%% fetch_output/2 reads each output with tflite_beam_tensor:to_binary/1, so what
%% comes back is the bytes, not the records this used to promise.
-spec predict(reference(), list(binary()) | binary() | map()) -> list(binary() | {error, binary()}) | {error, binary()}.
predict(Self, Input) when is_reference(Self) and (is_binary(Input) or is_list(Input) or is_map(Input)) ->
    case tflite_beam_interpreter:inputs(Self) of
        {ok, InputTensors} ->
            case tflite_beam_interpreter:outputs(Self) of
                {ok, OutputTensors} ->
                    case fill_input(Self, InputTensors, Input) of
                        ok ->
                            %% The result of the invoke decides whether the
                            %% output tensors mean anything. Dropping it meant
                            %% reading them anyway: a refused invoke, which is
                            %% what a second process sharing this interpreter
                            %% now gets, returned the previous run's answer to
                            %% whoever asked. Measured at fourteen wrong answers
                            %% in four hundred concurrent calls, which is the
                            %% same fault the interpreter guard was added to
                            %% close, arriving by a different door.
                            case tflite_beam_interpreter:invoke(Self) of
                                ok ->
                                    fetch_output(Self, OutputTensors);
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

%% Anything that is not one of those three shapes matched no clause and raised
%% from in here, which through the server took the server down. The elements
%% inside a list or a map are checked further in; this is the outer shape.
predict(Self, Input) when is_reference(Self) ->
    {error, unicode:characters_to_binary(
        io_lib:format("input must be a binary, a list of them, or a map, and this is ~p",
                      [Input]))}.

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
%% predict/2 takes any list or map at its outer guard, so an element that is not
%% binary data reached here and matched none of the clauses above. The caller got
%% function_clause, and a caller going through the server took the server down
%% with it, losing the interpreter it had loaded.
fill_input(Self, InputTensorIndex, InputData)
  when is_reference(Self) and is_integer(InputTensorIndex) ->
    {error, not_binary_reason(InputTensorIndex, InputData)};
fill_input(Self, InputTensors, InputMap) when is_reference(Self) and is_list(InputTensors) and is_map(InputMap) ->
    FillResults = lists:map(
        fun(InputTensorIndex) ->
            case tflite_beam_interpreter:tensor(Self, InputTensorIndex) of
                #tflite_beam_tensor{name = Name} = Tensor ->
                    HasInput = maps:is_key(Name, InputMap),
                    if 
                        HasInput ->
                            case maps:get(Name, InputMap) of
                                InputData when is_binary(InputData) ->
                                    tflite_beam_tensor:set_data(Tensor, InputData);
                                InputData ->
                                    not_binary_reason(InputTensorIndex, InputData)
                            end;
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

not_binary_reason(InputTensorIndex, InputData) ->
    Reason = io_lib:format(
        "input for tensor index ~w is ~p, which is not binary data",
        [InputTensorIndex, InputData]),
    unicode:characters_to_binary(Reason).

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

%% Collects what went wrong while filling the inputs.
%%
%% This used to treat every non-ok element as a bare binary and append it with
%% `R/binary', which raises badarg on anything else. What it is actually given is
%% whatever `tflite_beam_tensor:set_data/2' returned, and that is `{error,
%% Binary}'. So the one path that had a real reason to report crashed instead of
%% reporting it, and it crashed hardest exactly when it mattered: every refusal
%% from the interpreter guard arrives here.
not_ok_to_reason(Results) when is_list(Results) ->
    case [reason_of(R) || R <- Results, R =/= ok] of
        [] ->
            ok;
        Reasons ->
            {error, iolist_to_binary(lists:join(<<"; ">>, Reasons))}
    end.

reason_of({error, Reason}) when is_binary(Reason) -> Reason;
reason_of(Reason) when is_binary(Reason) -> Reason;
reason_of(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).

%% @doc
%% The inputs of the named signature, as a map of name to tensor index.
%%
%% An empty map is returned for a key the model does not declare.
-spec signature_inputs(reference(), binary() | list()) -> {ok, map()} | {error, binary()}.
signature_inputs(Self, SignatureKey) when is_reference(Self), is_list(SignatureKey) ->
    signature_inputs(Self, unicode:characters_to_binary(SignatureKey));
signature_inputs(Self, SignatureKey) when is_reference(Self), is_binary(SignatureKey) ->
    tflite_beam_nif:interpreter_signature_inputs(Self, SignatureKey).

%% @doc
%% The outputs of the named signature, as a map of name to tensor index.
%%
%% An empty map is returned for a key the model does not declare.
-spec signature_outputs(reference(), binary() | list()) -> {ok, map()} | {error, binary()}.
signature_outputs(Self, SignatureKey) when is_reference(Self), is_list(SignatureKey) ->
    signature_outputs(Self, unicode:characters_to_binary(SignatureKey));
signature_outputs(Self, SignatureKey) when is_reference(Self), is_binary(SignatureKey) ->
    tflite_beam_nif:interpreter_signature_outputs(Self, SignatureKey).

%% @doc
%% The subgraph a signature belongs to, or -1 for a key the model does not declare.
-spec get_subgraph_index_from_signature(reference(), binary() | list()) -> {ok, integer()} | {error, binary()}.
get_subgraph_index_from_signature(Self, SignatureKey) when is_reference(Self), is_list(SignatureKey) ->
    get_subgraph_index_from_signature(Self, unicode:characters_to_binary(SignatureKey));
get_subgraph_index_from_signature(Self, SignatureKey) when is_reference(Self), is_binary(SignatureKey) ->
    tflite_beam_nif:interpreter_get_subgraph_index_from_signature(Self, SignatureKey).

%% @doc
%% Allow a running `invoke/1' to be cancelled.
%%
%% Has to be called before invoking. Without it `cancel/1' is an error.
-spec enable_cancellation(reference()) -> ok | {error, binary()}.
enable_cancellation(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_enable_cancellation(Self).

%% @doc
%% Ask an in-flight `invoke/1' to stop.
%%
%% Does not block and is safe to call from another process, which is the point: an
%% invocation runs on a dirty scheduler and cannot otherwise be interrupted. Later
%% invocations are unaffected. Requires `enable_cancellation/1'.
-spec cancel(reference()) -> ok | {error, binary()}.
cancel(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_cancel(Self).

%% @doc
%% Release memory that is not needed between invocations.
%%
%% Invoking again reallocates it, so this trades time for memory on devices short of
%% the latter.
-spec release_non_persistent_memory(reference()) -> ok | {error, binary()}.
release_non_persistent_memory(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_release_non_persistent_memory(Self).

%% @doc
%% Reset all variable tensors to zero.
%%
%% `tflite_beam_tflite:reset_variable_tensor/1' resets a single one.
-spec reset_variable_tensors(reference()) -> ok | {error, binary()}.
reset_variable_tensors(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_reset_variable_tensors(Self).

%% @doc
%% How many subgraphs the model has.
-spec subgraphs_size(reference()) -> {ok, non_neg_integer()} | {error, binary()}.
subgraphs_size(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_subgraphs_size(Self).

%% @doc
%% Whether float32 operations may be carried out in float16.
-spec get_allow_fp16_precision_for_fp32(reference()) -> {ok, boolean()} | {error, binary()}.
get_allow_fp16_precision_for_fp32(Self) when is_reference(Self) ->
    tflite_beam_nif:interpreter_get_allow_fp16_precision_for_fp32(Self).

%% @doc
%% Allow or forbid carrying out float32 operations in float16.
%%
%% Only has an effect on backends that can do it, and has to be set before the graph
%% is prepared.
-spec set_allow_fp16_precision_for_fp32(reference(), boolean()) -> ok | {error, binary()}.
set_allow_fp16_precision_for_fp32(Self, Allow) when is_reference(Self), is_boolean(Allow) ->
    tflite_beam_nif:interpreter_set_allow_fp16_precision_for_fp32(Self, Allow).
