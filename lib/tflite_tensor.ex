defmodule TFLiteElixir.TfLiteTensor do
  import TFLiteElixir.Errorize

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

  defstruct [:name, :index, :shape, :shape_signature, :type, :quantization_params, :sparsity_params, :reference]
  alias __MODULE__, as: T

  @doc """
  Get the data type
  """
  @spec type(reference()) :: tensor_type() | nif_error()
  def type(self) when is_reference(self) do
    TFLiteElixir.Nif.tflitetensor_type(self)
  end

  @spec type(%T{}) :: tensor_type()
  def type(%T{type: type}), do: type

  deferror type(self)

  @doc """
  Get the dimensions
  """
  @spec dims(reference()) :: {:ok, [integer()]} | nif_error()
  def dims(self) do
    TFLiteElixir.Nif.tflitetensor_dims(self)
  end

  @spec dims(%T{}) :: [integer()]
  def dims(%T{shape: shape}), do: shape

  deferror dims(self)

  @doc """
  Get the quantization params
  """
  def quantization_params(self) do
    TFLiteElixir.Nif.tflitetensor_quantization_params(self)
  end

  def quantization_params(%T{quantization_params: quantization_params}), do: quantization_params

  deferror quantization_params(self)
end
