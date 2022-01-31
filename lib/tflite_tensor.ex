defmodule TFLite.TfLiteTensor do
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

  @spec type(reference()) :: tensor_type() | nif_error()
  def type(self) do
    TFLite.Nif.tflitetensor_type(self)
  end

  @spec dims(reference()) :: {:ok, [integer()]} | nif_error()
  def dims(self) do
    TFLite.Nif.tflitetensor_dims(self)
  end
end
