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

  @doc """
  Get the data type
  """
  @spec type(reference()) :: tensor_type() | nif_error()
  def type(self) do
    TFLiteElixir.Nif.tflitetensor_type(self)
  end

  deferror type(self)

  @doc """
  Get the dimensions
  """
  @spec dims(reference()) :: {:ok, [integer()]} | nif_error()
  def dims(self) do
    TFLiteElixir.Nif.tflitetensor_dims(self)
  end

  deferror dims(self)
end
