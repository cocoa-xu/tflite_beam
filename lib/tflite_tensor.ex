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
  @spec type(%T{}) :: tensor_type()
  def type(%T{type: type}), do: type

  @spec type(reference()) :: tensor_type() | nif_error()
  def type(self) when is_reference(self) do
    TFLiteElixir.Nif.tflitetensor_type(self)
  end

  deferror type(self)

  @doc """
  Get the dimensions
  """
  @spec dims(%T{}) :: [integer()]
  def dims(%T{shape: shape}), do: shape

  @spec dims(reference()) :: {:ok, [integer()]} | nif_error()
  def dims(self) do
    TFLiteElixir.Nif.tflitetensor_dims(self)
  end

  deferror dims(self)

  @doc """
  Get the quantization params
  """
  def quantization_params(%T{quantization_params: quantization_params}), do: quantization_params

  def quantization_params(self) do
    TFLiteElixir.Nif.tflitetensor_quantization_params(self)
  end

  deferror quantization_params(self)

  @doc """
  Set tensor data
  """
  def set_data(%T{reference: reference}, data), do: set_data(reference, data)

  def set_data(self, %Nx.Tensor{}=data) when is_reference(self) do
    TFLiteElixir.Nif.tflitetensor_set_data(self, Nx.to_binary(data))
  end

  def set_data(self, data) when is_reference(self) and is_binary(data) do
    TFLiteElixir.Nif.tflitetensor_set_data(self, data)
  end

  deferror set_data(self, data)

  @doc """
  Get binary data
  """
  @spec to_binary(%T{}) :: binary()
  def to_binary(%T{reference: reference}), do: to_binary(reference)

  @spec to_binary(reference()) :: binary()
  def to_binary(self) when is_reference(self) do
    TFLiteElixir.Nif.tflitetensor_to_binary(self)
  end

  deferror to_binary(self)

  @doc """
  To Nx.Tensor
  """
  @spec to_nx(%T{}) :: binary()
  def to_nx(%T{}=self) do
    Nx.from_binary(to_binary!(self), self.type)
    |> Nx.reshape(List.to_tuple(self.shape))
  end

  @spec to_nx(reference()) :: binary()
  def to_nx(self) when is_reference(self) do
    Nx.from_binary(to_binary!(self), type!(self))
    |> Nx.reshape(List.to_tuple(dims!(self)))
  end

  deferror to_nx(self)
end
