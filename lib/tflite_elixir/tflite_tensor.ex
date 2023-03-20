defmodule TFLiteElixir.TFLiteTensor do
  @moduledoc """
  A typed multi-dimensional array used in Tensorflow Lite.
  """
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

  defstruct [
    :name,
    :index,
    :shape,
    :shape_signature,
    :type,
    :quantization_params,
    :sparsity_params,
    :reference
  ]

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

  deferror(type(self))

  @doc """
  Get the dimensions
  """
  @spec dims(%T{}) :: [integer()]
  def dims(%T{shape: shape}), do: shape

  @spec dims(reference()) :: {:ok, [integer()]} | nif_error()
  def dims(self) do
    TFLiteElixir.Nif.tflitetensor_dims(self)
  end

  deferror(dims(self))

  @doc """
  Get the quantization params
  """
  def quantization_params(%T{quantization_params: quantization_params}), do: quantization_params

  def quantization_params(self) do
    TFLiteElixir.Nif.tflitetensor_quantization_params(self)
  end

  deferror(quantization_params(self))

  @doc """
  Set tensor data
  """
  def set_data(%T{reference: reference}, data), do: set_data(reference, data)

  def set_data(self, %Nx.Tensor{} = data) when is_reference(self) do
    TFLiteElixir.Nif.tflitetensor_set_data(self, Nx.to_binary(data))
  end

  def set_data(self, data) when is_reference(self) and is_binary(data) do
    TFLiteElixir.Nif.tflitetensor_set_data(self, data)
  end

  deferror(set_data(self, data))

  @doc """
  Get binary data
  """
  def to_binary(self, limit \\ 0)

  @spec to_binary(%T{}, non_neg_integer()) :: {:ok, binary()} | {:error, String.t()}
  def to_binary(%T{reference: reference}, limit) when limit >= 0 do
    TFLiteElixir.Nif.tflitetensor_to_binary(reference, limit)
  end

  @spec to_binary(reference(), non_neg_integer()) :: {:ok, binary()} | {:error, String.t()}
  def to_binary(self, limit) when is_reference(self) and limit >= 0 do
    TFLiteElixir.Nif.tflitetensor_to_binary(self, limit)
  end

  @doc false
  # Convert `Nx.Tensor` to `TFLiteElixir.TFLiteTensor`
  def from_nx(%Nx.Tensor{} = _tensor) do
    raise "not implemented"
  end

  @doc """
  Convert `TFLiteElixir.TFLiteTensor` to `Nx.Tensor`
  """
  @spec to_nx(reference() | %T{}, Keyword.t()) :: %Nx.Tensor{}
  def to_nx(self_struct, opts \\ [])

  def to_nx(self_struct, opts) when is_struct(self_struct, T) and is_list(opts) do
    type = type(self_struct)
    shape = List.to_tuple(dims(self_struct))

    with {:ok, binary} <- to_binary(self_struct) do
      Nx.from_binary(binary, type, backend: opts[:backend])
      |> Nx.reshape(shape)
    else
      error -> error
    end
  end

  def to_nx(self, opts) when is_reference(self) and is_list(opts) do
    type = type!(self)
    shape = List.to_tuple(dims!(self))

    with {:ok, binary} <- to_binary(self) do
      Nx.from_binary(binary, type, backend: opts[:backend])
      |> Nx.reshape(shape)
    else
      error -> error
    end
  end
end
