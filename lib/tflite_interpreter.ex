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

  @spec new() :: nif_resource_ok() | nif_error()
  def new() do
    TFLite.Nif.interpreter_new()
  end

  @spec new!() :: reference()
  def new!() do
    {:ok, interpreter} = new()
    interpreter
  end

  @spec allocateTensors(reference()) :: :ok | nif_error()
  def allocateTensors(self) do
    TFLite.Nif.interpreter_allocateTensors(self)
  end

  @spec inputs(reference()) :: {:ok, [non_neg_integer()]} | nif_error()
  def inputs(self) do
    TFLite.Nif.interpreter_inputs(self)
  end

  @spec getInputName(reference(), non_neg_integer()) :: {:ok, String.t()} | nif_error()
  def getInputName(self, index) do
    TFLite.Nif.interpreter_getInputName(self, index)
  end

  @spec input_tensor(reference(), non_neg_integer(), binary()) :: :ok | nif_error()
  def input_tensor(self, index, data) do
    TFLite.Nif.interpreter_input_tensor(self, index, data)
  end

  @spec invoke(reference()) :: :ok | nif_error()
  def invoke(self) do
    TFLite.Nif.interpreter_invoke(self)
  end

  @spec outputs(reference()) :: {:ok, [non_neg_integer()]} | nif_error()
  def outputs(self) do
    TFLite.Nif.interpreter_outputs(self)
  end

  @spec getOutputName(reference(), non_neg_integer()) :: {:ok, String.t()} | nif_error()
  def getOutputName(self, index) do
    TFLite.Nif.interpreter_getOutputName(self, index)
  end

  @spec output_tensor(reference(), non_neg_integer()) ::
          {:ok, tensor_type(), binary()} | nif_error()
  def output_tensor(self, index) do
    TFLite.Nif.interpreter_output_tensor(self, index)
  end

  @spec tensor(reference(), non_neg_integer()) :: {:ok, reference()} | nif_error()
  def tensor(self, tensor_index) do
    TFLite.Nif.interpreter_tensor(self, tensor_index)
  end
end
