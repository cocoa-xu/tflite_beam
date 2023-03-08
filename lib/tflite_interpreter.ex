defmodule TFLiteElixir.Interpreter do
  import TFLiteElixir.Errorize
  alias TFLiteElixir.TFLiteTensor, as: Tensor
  alias TFLiteElixir.TFLiteQuantizationParams, as: TFLiteQuantizationParams

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
    with {:build_from_file, %TFLiteElixir.FlatBufferModel{} = model} <-
           {:build_from_file, TFLiteElixir.FlatBufferModel.buildFromFile(model_path)},
         {:builtin_resolver, {:ok, resolver}} <-
           {:builtin_resolver, TFLiteElixir.Ops.Builtin.BuiltinResolver.new()},
         {:interpreter_build, {:ok, builder}} <-
           {:interpreter_build, TFLiteElixir.InterpreterBuilder.new(model, resolver)},
         {:new_interpreter, {:ok, interpreter}} <-
           {:new_interpreter, TFLiteElixir.Interpreter.new()},
         {:build_interpreter, :ok} <-
           {:build_interpreter, TFLiteElixir.InterpreterBuilder.build(builder, interpreter)},
         {:allocate_tensors, :ok} <-
           {:allocate_tensors, TFLiteElixir.Interpreter.allocateTensors(interpreter)} do
      {:ok, interpreter}
    else
      error -> error
    end
  end

  deferror(new(model_path))

  def predict(interpreter, input) do
    with {:ok, input_tensors} <- TFLiteElixir.Interpreter.inputs(interpreter),
         {:ok, output_tensors} <- TFLiteElixir.Interpreter.outputs(interpreter),
         :ok <- fill_input(interpreter, input_tensors, input) do
      TFLiteElixir.Interpreter.invoke(interpreter)
      fetch_output(interpreter, output_tensors)
    else
      error -> error
    end
  end

  defp fill_input(interpreter, input_tensors, input)
       when is_list(input_tensors) and is_list(input) do
    if length(input_tensors) == length(input) do
      Enum.zip_with([input_tensors, input], fn [input_index, input_tensor] ->
        fill_input(interpreter, input_index, input_tensor)
      end)
    end
  end

  defp fill_input(interpreter, input_tensors, %Nx.Tensor{} = input)
       when is_list(input_tensors) and length(input_tensors) == 1 do
    [tensor_index] = input_tensors
    fill_input(interpreter, tensor_index, input)
  end

  defp fill_input(interpreter, input_tensor_index, %Nx.Tensor{} = input)
       when is_integer(input_tensor_index) do
    tensor = TFLiteElixir.Interpreter.tensor!(interpreter, input_tensor_index)

    with {:match_type, _, _, true} <-
           {:match_type, tensor.type, Nx.type(input), tensor.type == Nx.type(input)},
         {:match_shape, _, _, true} <-
           {:match_shape, List.to_tuple(tensor.shape), Nx.shape(input),
            tensor.shape == Tuple.to_list(Nx.shape(input)) or
              tensor.shape == [1 | Tuple.to_list(Nx.shape(input))]} do
      Tensor.set_data(tensor, Nx.to_binary(input))
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
    with {:ok, tensor} <- TFLiteElixir.Interpreter.tensor(interpreter, input_tensor_index) do
      Tensor.set_data(tensor, input)
    else
      error -> error
    end
  end

  defp fill_input(interpreter, input_tensors, input)
       when is_list(input_tensors) and is_map(input) do
    ret =
      Enum.map(input_tensors, fn input_tensor_index ->
        {:ok, out_tensor} = TFLiteElixir.Interpreter.tensor(interpreter, input_tensor_index)
        name = out_tensor.name
        data = Map.get(input, name, nil)

        if data do
          fill_input(out_tensor, data)
          :ok
        else
          "missing input data for tensor #{name}, tensor index: #{input_tensor_index}"
        end
      end)
      |> Enum.reject(fn r -> r == :ok end)

    if ret == [] do
      :ok
    else
      {:error, Enum.join(ret, "; ")}
    end
  end

  defp fill_input(%Tensor{} = tensor, input)
       when is_binary(input) do
    Tensor.set_data(tensor, input)
  end

  defp fill_input(%Tensor{} = tensor, %Nx.Tensor{} = input) do
    Tensor.set_data(tensor, Nx.to_binary(input))
  end

  defp fetch_output(interpreter, output_tensors)
       when is_list(output_tensors) do
    Enum.map(output_tensors, fn output_index ->
      fetch_output(interpreter, output_index)
    end)
  end

  defp fetch_output(interpreter, output_index) when is_integer(output_index) do
    with {:ok, tensor} <- TFLiteElixir.Interpreter.tensor(interpreter, output_index) do
      Tensor.to_nx(tensor)
    else
      error -> error
    end
  end

  @doc """
  Allocate memory for tensors in the graph
  """
  @spec allocateTensors(reference()) :: :ok | nif_error()
  def allocateTensors(self) when is_reference(self) do
    TFLiteElixir.Nif.interpreter_allocateTensors(self)
  end

  deferror(allocateTensors(self))

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
  @spec getInputName(reference(), non_neg_integer()) :: {:ok, String.t()} | nif_error()
  def getInputName(self, index) when is_reference(self) and index >= 0 do
    TFLiteElixir.Nif.interpreter_getInputName(self, index)
  end

  deferror(getInputName(self, index))

  @doc """
  Fill data to the specified input tensor

  Note: although we have `typed_input_tensor` in the C++ end, but here what we really passed
  to the NIF is `binary` data, therefore, I'm not pretend that we have type information.

  ## Example: Get the expected data type and shape for the input tensor
  ```elixir
  {:ok, tensor} = TFLite.Interpreter.tensor(interpreter, 0)
  {:ok, [1, 224, 224, 3]} = TFLite.TFLiteTensor.dims(tensor)
  {:u, 8} = TFLite.TFLiteTensor.type(tensor)
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
  Get the list of output tensors.

  return a list of output tensor id
  """
  @spec getOutputName(reference(), non_neg_integer()) :: {:ok, String.t()} | nif_error()
  def getOutputName(self, index) when is_reference(self) and index >= 0 do
    TFLiteElixir.Nif.interpreter_getOutputName(self, index)
  end

  deferror(getOutputName(self, index))

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
  @spec tensor(reference(), non_neg_integer()) :: {:ok, %Tensor{}} | nif_error()
  def tensor(self, tensor_index) when is_reference(self) and tensor_index >= 0 do
    with {:ok,
          {name, index, shape, shape_signature, type, {scale, zero_point, quantized_dimension},
           sparsity_params, ref}} <- TFLiteElixir.Nif.interpreter_tensor(self, tensor_index) do
      {:ok,
       %Tensor{
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
       }}
    else
      e -> e
    end
  end

  deferror(tensor(self, tensor_index))

  @doc """
  Set the number of threads available to the interpreter.

  NOTE: num_threads should be >= -1. Setting num_threads to 0 has the effect
  to disable multithreading, which is equivalent to setting num_threads
  to 1. If set to the value -1, the number of threads used will be
  implementation-defined and platform-dependent.

  As TfLite interpreter could internally apply a TfLite delegate by default
  (i.e. XNNPACK), the number of threads that are available to the default
  delegate *should be* set via InterpreterBuilder APIs as follows:

  ```elixir
  interpreter = TFLiteElixir.Interpreter.new!()
  builder = TFLiteElixir.InterpreterBuilder.new!(tflite model, op resolver)
  TFLiteElixir.InterpreterBuilder.setNumThreads(builder, ...)
  assert :ok == TFLiteElixir.InterpreterBuilder.build!(builder, interpreter)
  ```
  """
  @spec setNumThreads(reference(), integer()) :: :ok | nif_error()
  def setNumThreads(self, num_threads) when is_integer(num_threads) and num_threads >= -1 do
    TFLiteElixir.Nif.interpreter_setNumThreads(self, num_threads)
  end

  deferror(setNumThreads(self, num_threads))

  @spec getSignatureDefs(reference()) :: Map.t()
  def getSignatureDefs(self) do
    TFLiteElixir.Nif.interpreter_get_signature_defs(self)
  end

  deferror(getSignatureDefs(self))

  @spec get_full_signature_list(reference()) :: Map.t()
  def get_full_signature_list(self) do
    getSignatureDefs(self)
  end

  deferror(get_full_signature_list(self))
end
