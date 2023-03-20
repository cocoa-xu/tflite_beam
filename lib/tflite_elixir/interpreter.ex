defmodule TFLiteElixir.Interpreter do
  @moduledoc """
  An interpreter for a graph of nodes that input and output from tensors.
  """
  import TFLiteElixir.Errorize

  alias TFLiteElixir.TFLiteTensor
  alias TFLiteElixir.TFLiteQuantizationParams
  alias TFLiteElixir.FlatBufferModel
  alias TFLiteElixir.Interpreter
  alias TFLiteElixir.InterpreterBuilder

  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}
  @type tensor_type ::
          :no_type
          | {:f, 32}
          | {:s, 32}
          | {:u, 8}
          | {:s, 64}
          | :string
          | :bool
          | {:s, 16}
          | {:c, 64}
          | {:s, 8}
          | {:f, 16}
          | {:f, 64}
          | {:c, 128}
          | {:u, 64}
          | :resource
          | :variant
          | {:u, 32}

  @doc """
  New interpreter
  """
  @spec new() :: nif_resource_ok() | nif_error()
  def new() do
    TFLiteElixir.Nif.interpreter_new()
  end

  deferror(new())

  @doc """
  New interpreter with model
  """
  @spec new(String.t()) :: nif_resource_ok() | nif_error()
  def new(model_path) do
    with {:build_from_file, %FlatBufferModel{} = model} <-
           {:build_from_file, FlatBufferModel.build_from_file(model_path)},
         {:builtin_resolver, {:ok, resolver}} <-
           {:builtin_resolver, TFLiteElixir.Ops.Builtin.BuiltinResolver.new()},
         {:interpreter_build, {:ok, builder}} <-
           {:interpreter_build, InterpreterBuilder.new(model, resolver)},
         {:new_interpreter, {:ok, interpreter}} <-
           {:new_interpreter, Interpreter.new()},
         {:build_interpreter, :ok} <-
           {:build_interpreter, InterpreterBuilder.build(builder, interpreter)},
         {:allocate_tensors, :ok} <-
           {:allocate_tensors, Interpreter.allocate_tensors(interpreter)} do
      {:ok, interpreter}
    else
      error -> error
    end
  end

  deferror(new(model_path))

  @doc """
  Provide a list of tensor indexes that are inputs to the model.
  Each index is bound check and this modifies the consistent_ flag of the
  interpreter.
  """
  @spec set_inputs(reference, list(integer())) :: :ok | nif_error()
  def set_inputs(self, inputs) when is_reference(self) and is_list(inputs) do
    TFLiteElixir.Nif.interpreter_set_inputs(self, inputs)
  end

  @doc """
  Provide a list of tensor indexes that are outputs to the model.
  Each index is bound check and this modifies the consistent_ flag of the
  interpreter.
  """
  @spec set_outputs(reference, list(integer())) :: :ok | nif_error()
  def set_outputs(self, outputs) when is_reference(self) and is_list(outputs) do
    TFLiteElixir.Nif.interpreter_set_outputs(self, outputs)
  end

  @doc """
  Provide a list of tensor indexes that are variable tensors.
  Each index is bound check and this modifies the consistent_ flag of the
  interpreter.
  """
  @spec set_variables(reference, list(integer())) :: :ok | nif_error()
  def set_variables(self, variables) when is_reference(self) and is_list(variables) do
    TFLiteElixir.Nif.interpreter_set_variables(self, variables)
  end

  @doc """
  Fill input data to corresponding input tensor of the interpreter,
  call `Interpreter.invoke` and return output tensor(s)
  """
  def predict(interpreter, input) do
    with {:ok, input_tensors} <- Interpreter.inputs(interpreter),
         {:ok, output_tensors} <- Interpreter.outputs(interpreter),
         :ok <- fill_input(interpreter, input_tensors, input) do
      Interpreter.invoke(interpreter)
      fetch_output(interpreter, output_tensors)
    else
      error -> error
    end
  end

  defp fill_input(interpreter, input_tensors, input)
       when is_list(input_tensors) and is_list(input) do
    if length(input_tensors) == length(input) do
      fill_results =
        Enum.zip_with([input_tensors, input], fn [input_index, input_tensor] ->
          fill_input(interpreter, input_index, input_tensor)
        end)
      all_filled = Enum.all?(fill_results, fn r -> r == :ok end)
      if all_filled do
        :ok
      else
        Enum.reject(fill_results, fn x -> x == :ok end)
      end
    else
      {:error, "length mismatch: there are #{length(input_tensors)} input tensors while the input list has #{length(input)} elements"}
    end
  end

  defp fill_input(interpreter, input_tensors, %Nx.Tensor{} = input)
       when is_list(input_tensors) and length(input_tensors) == 1 do
    [tensor_index] = input_tensors
    fill_input(interpreter, tensor_index, input)
  end

  defp fill_input(interpreter, input_tensor_index, %Nx.Tensor{} = input)
       when is_integer(input_tensor_index) do
    %TFLiteTensor{} = tensor = Interpreter.tensor(interpreter, input_tensor_index)

    with {:match_type, _, _, true} <-
           {:match_type, tensor.type, Nx.type(input), tensor.type == Nx.type(input)},
         {:match_shape, _, _, true} <-
           {:match_shape, List.to_tuple(tensor.shape), Nx.shape(input),
            tensor.shape == Tuple.to_list(Nx.shape(input)) or
              tensor.shape == [1 | Tuple.to_list(Nx.shape(input))]} do
      TFLiteTensor.set_data(tensor, Nx.to_binary(input))
    else
      {:match_type, tensor_type, input_type, _} ->
        {:error,
         "input data type, #{inspect(input_type)}, does not match the data type of the tensor, #{inspect(tensor_type)}, tensor index: #{input_tensor_index}"}

      {:match_shape, tensor_shape, input_shape, _} ->
        {:error,
         "input data shape, #{inspect(input_shape)}, does not match the shape type of the tensor, #{inspect(tensor_shape)}, tensor index: #{input_tensor_index}"}

      error ->
        error
    end
  end

  defp fill_input(interpreter, input_tensor_index, input)
       when is_integer(input_tensor_index) and is_binary(input) do
    case Interpreter.tensor(interpreter, input_tensor_index) do
      %TFLiteTensor{} = tensor ->
        TFLiteTensor.set_data(tensor, input)

      error ->
        error
    end
  end

  defp fill_input(interpreter, input_tensors, input)
       when is_list(input_tensors) and is_map(input) do
    ret =
      Enum.map(input_tensors, fn input_tensor_index ->
        %TFLiteTensor{} = out_tensor = Interpreter.tensor(interpreter, input_tensor_index)
        name = out_tensor.name
        data = Map.get(input, name, nil)

        if data do
          fill_input(out_tensor, data)
          :ok
        else
          "missing input data for tensor `#{name}`, tensor index: #{input_tensor_index}"
        end
      end)
      |> Enum.reject(fn r -> r == :ok end)

    if ret == [] do
      :ok
    else
      {:error, Enum.join(ret, "; ")}
    end
  end

  defp fill_input(%TFLiteTensor{} = tensor, input)
       when is_binary(input) do
    TFLiteTensor.set_data(tensor, input)
  end

  defp fill_input(%TFLiteTensor{} = tensor, %Nx.Tensor{} = input) do
    TFLiteTensor.set_data(tensor, Nx.to_binary(input))
  end

  defp fetch_output(interpreter, output_tensors)
       when is_list(output_tensors) do
    Enum.map(output_tensors, fn output_index ->
      fetch_output(interpreter, output_index)
    end)
  end

  defp fetch_output(interpreter, output_index) when is_integer(output_index) do
    case Interpreter.tensor(interpreter, output_index) do
      %TFLiteTensor{} = tensor ->
        TFLiteTensor.to_nx(tensor)

      error ->
        error
    end
  end

  @doc """
  Allocate memory for tensors in the graph
  """
  @spec allocate_tensors(reference()) :: :ok | nif_error()
  def allocate_tensors(self) when is_reference(self) do
    TFLiteElixir.Nif.interpreter_allocate_tensors(self)
  end

  deferror(allocate_tensors(self))

  @doc """
  Get the list of input tensors.

  return a list of input tensor id
  """
  @spec inputs(reference()) :: {:ok, [non_neg_integer()]} | nif_error()
  def inputs(self) when is_reference(self) do
    TFLiteElixir.Nif.interpreter_inputs(self)
  end

  deferror(inputs(self))

  @doc """
  Get the name of the input tensor

  Note that the index here means the index in the result list of `inputs/1`. For example,
  if `inputs/1` returns `[42, 314]`, then `0` should be passed here to get the name of
  tensor `42`
  """
  @spec get_input_name(reference(), non_neg_integer()) :: {:ok, String.t()} | nif_error()
  def get_input_name(self, index) when is_reference(self) and index >= 0 do
    TFLiteElixir.Nif.interpreter_get_input_name(self, index)
  end

  deferror(get_input_name(self, index))

  @doc """
  Fill data to the specified input tensor

  Note: although we have `typed_input_tensor` in the C++ end, but here what we really passed
  to the NIF is `binary` data, therefore, I'm not pretend that we have type information.

  ## Example: Get the expected data type and shape for the input tensor
  ```elixir
  %TFLiteTensor{} = tensor = Interpreter.tensor(interpreter, 0)
  {:ok, [1, 224, 224, 3]} = TFLiteTensor.dims(tensor)
  {:u, 8} = TFLiteTensor.type(tensor)
  ```
  """
  @spec input_tensor(reference(), non_neg_integer(), binary()) :: :ok | nif_error()
  def input_tensor(self, index, data)
      when is_reference(self) and index >= 0 and is_binary(data) do
    TFLiteElixir.Nif.interpreter_input_tensor(self, index, data)
  end

  deferror(input_tensor(self, index, data))

  @doc """
  Run forwarding
  """
  @spec invoke(reference()) :: :ok | nif_error()
  def invoke(self) when is_reference(self) do
    TFLiteElixir.Nif.interpreter_invoke(self)
  end

  deferror(invoke(self))

  @doc """
  Get the list of output tensors.

  return a list of output tensor id
  """
  @spec outputs(reference()) :: {:ok, [non_neg_integer()]} | nif_error()
  def outputs(self) when is_reference(self) do
    TFLiteElixir.Nif.interpreter_outputs(self)
  end

  deferror(outputs(self))

  @doc """
  Get the list of variable tensors.
  """
  @spec variables(reference()) :: {:ok, [non_neg_integer()]} | nif_error()
  def variables(self) when is_reference(self) do
    TFLiteElixir.Nif.interpreter_variables(self)
  end

  @doc """
  Get the list of output tensors.

  return a list of output tensor id
  """
  @spec get_output_name(reference(), non_neg_integer()) :: {:ok, String.t()} | nif_error()
  def get_output_name(self, index) when is_reference(self) and index >= 0 do
    TFLiteElixir.Nif.interpreter_get_output_name(self, index)
  end

  deferror(get_output_name(self, index))

  @doc """
  Get the name of the input tensor

  Note that the index here means the index in the result list of `outputs/1`. For example,
  if `outputs/1` returns `[42, 314]`, then `0` should be passed here to get the name of
  tensor `42`
  """
  @spec output_tensor(reference(), non_neg_integer()) ::
          {:ok, tensor_type(), binary()} | nif_error()
  def output_tensor(self, index) when is_reference(self) and index >= 0 do
    TFLiteElixir.Nif.interpreter_output_tensor(self, index)
  end

  deferror(output_tensor(self, index))

  @doc """
  Get any tensor in the graph by its id

  Note that the `tensor_index` here means the id of a tensor. For example,
  if `inputs/1` returns `[42, 314]`, then `42` should be passed here to get tensor `42`.
  """
  @spec tensor(reference(), non_neg_integer()) :: %TFLiteTensor{} | nif_error()
  def tensor(self, tensor_index) when is_reference(self) and tensor_index >= 0 do
    with {:ok,
          {name, index, shape, shape_signature, type, {scale, zero_point, quantized_dimension},
           sparsity_params, ref}} <- TFLiteElixir.Nif.interpreter_tensor(self, tensor_index) do
      %TFLiteTensor{
        name: name,
        index: index,
        shape: shape,
        shape_signature: shape_signature,
        type: type,
        quantization_params: %TFLiteQuantizationParams{
          scale: scale,
          zero_point: zero_point,
          quantized_dimension: quantized_dimension
        },
        sparsity_params: sparsity_params,
        reference: ref
      }
    else
      e -> e
    end
  end

  @doc """
  Set the number of threads available to the interpreter.

  NOTE: num_threads should be >= 1.

  As TfLite interpreter could internally apply a TfLite delegate by default
  (i.e. XNNPACK), the number of threads that are available to the default
  delegate *should be* set via InterpreterBuilder APIs as follows:

  ```elixir
  interpreter = Interpreter.new!()
  builder = InterpreterBuilder.new!(tflite model, op resolver)
  InterpreterBuilder.set_num_threads(builder, ...)
  assert :ok == InterpreterBuilder.build!(builder, interpreter)
  ```
  """
  @spec set_num_threads(reference(), integer()) :: :ok | nif_error()
  def set_num_threads(self, num_threads) when is_integer(num_threads) and num_threads >= 1 do
    TFLiteElixir.Nif.interpreter_set_num_threads(self, num_threads)
  end

  deferror(set_num_threads(self, num_threads))

  @doc """
  Get SignatureDef dict from the Metadata of a TfLite flatbuffer buffer.

  `self`: `TFLiteElixir.Interpreter`

    TFLite model buffer to get the signature_def.

  ##### Returns:

  Map containing serving names to SignatureDefs if exists, otherwise, `nil`.
  """
  @spec get_signature_defs(reference()) :: {:ok, Map.t()} | nil | {:error, String.t()}
  def get_signature_defs(self) do
    TFLiteElixir.Nif.interpreter_get_signature_defs(self)
  end

  deferror(get_signature_defs(self))
end
