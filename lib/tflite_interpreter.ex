defmodule TFLite.Interpreter do
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
    TFLite.Nif.interpreter_new()
  end

  @doc """
  New interpreter
  """
  @spec new!() :: reference()
  def new!() do
    {:ok, interpreter} = new()
    interpreter
  end

  @doc """
  New interpreter with model
  """
  @spec new(String.t()) :: nif_resource_ok() | nif_error()
  def new(model_path) do
    with {:build_from_file, {:ok, model}} <- {:build_from_file, TFLite.FlatBufferModel.buildFromFile(model_path)},
         {:builtin_resolver, {:ok, resolver}} <- {:builtin_resolver, TFLite.Ops.Builtin.BuiltinResolver.new()},
         {:interpreter_build, {:ok, builder}} <- {:interpreter_build, TFLite.InterpreterBuilder.new(model, resolver)},
         {:new_interpreter, {:ok, interpreter}} <- {:new_interpreter, TFLite.Interpreter.new()},
         {:build_interpreter, :ok} <- {:build_interpreter, TFLite.InterpreterBuilder.build(builder, interpreter)},
         {:allocate_tensors, :ok} <- {:allocate_tensors, TFLite.Interpreter.allocateTensors(interpreter)} do
      {:ok, interpreter}
    else
      error -> error
    end
  end

  @doc """
  New interpreter with model
  """
  @spec new!(String.t()) :: reference()
  def new!(model_path) do
    {:ok, interpreter} = new(model_path)
    interpreter
  end

  @doc """
  Allocate memory for tensors in the graph
  """
  @spec allocateTensors(reference()) :: :ok | nif_error()
  def allocateTensors(self) when is_reference(self) do
    TFLite.Nif.interpreter_allocateTensors(self)
  end

  @doc """
  Get the list of input tensors.

  return a list of input tensor id
  """
  @spec inputs(reference()) :: {:ok, [non_neg_integer()]} | nif_error()
  def inputs(self) when is_reference(self) do
    TFLite.Nif.interpreter_inputs(self)
  end

  @doc """
  Get the name of the input tensor

  Note that the index here means the index in the result list of `inputs/1`. For example,
  if `inputs/1` returns `[42, 314]`, then `0` should be passed here to get the name of
  tensor `42`
  """
  @spec getInputName(reference(), non_neg_integer()) :: {:ok, String.t()} | nif_error()
  def getInputName(self, index) when is_reference(self) and index >= 0 do
    TFLite.Nif.interpreter_getInputName(self, index)
  end

  @doc """
  Fill data to the specified input tensor

  Note: although we have `typed_input_tensor` in the C++ end, but here what we really passed
  to the NIF is `binary` data, therefore, I'm not pretend that we have type information.

  ## Example: Get the expected data type and shape for the input tensor
  ```elixir
  {:ok, tensor} = TFLite.Interpreter.tensor(interpreter, 0)
  {:ok, [1, 224, 224, 3]} = TFLite.TfLiteTensor.dims(tensor)
  {:u, 8} = TFLite.TfLiteTensor.type(tensor)
  ```
  """
  @spec input_tensor(reference(), non_neg_integer(), binary()) :: :ok | nif_error()
  def input_tensor(self, index, data)
      when is_reference(self) and index >= 0 and is_binary(data) do
    TFLite.Nif.interpreter_input_tensor(self, index, data)
  end

  @doc """
  Run forwarding
  """
  @spec invoke(reference()) :: :ok | nif_error()
  def invoke(self) when is_reference(self) do
    TFLite.Nif.interpreter_invoke(self)
  end

  @doc """
  Get the list of output tensors.

  return a list of output tensor id
  """
  @spec outputs(reference()) :: {:ok, [non_neg_integer()]} | nif_error()
  def outputs(self) when is_reference(self) do
    TFLite.Nif.interpreter_outputs(self)
  end

  @doc """
  Get the list of output tensors.

  return a list of output tensor id
  """
  @spec getOutputName(reference(), non_neg_integer()) :: {:ok, String.t()} | nif_error()
  def getOutputName(self, index) when is_reference(self) and index >= 0 do
    TFLite.Nif.interpreter_getOutputName(self, index)
  end

  @doc """
  Get the name of the input tensor

  Note that the index here means the index in the result list of `outputs/1`. For example,
  if `outputs/1` returns `[42, 314]`, then `0` should be passed here to get the name of
  tensor `42`
  """
  @spec output_tensor(reference(), non_neg_integer()) ::
          {:ok, tensor_type(), binary()} | nif_error()
  def output_tensor(self, index) when is_reference(self) and index >= 0 do
    TFLite.Nif.interpreter_output_tensor(self, index)
  end

  @doc """
  Get any tensor in the graph by its id

  Note that the `tensor_index` here means the id of a tensor. For example,
  if `inputs/1` returns `[42, 314]`, then `42` should be passed here to get tensor `42`.
  """
  @spec tensor(reference(), non_neg_integer()) :: {:ok, reference()} | nif_error()
  def tensor(self, tensor_index) when is_reference(self) and tensor_index >= 0 do
    TFLite.Nif.interpreter_tensor(self, tensor_index)
  end
end
