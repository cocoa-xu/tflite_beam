defmodule TFLiteBEAM.TFLiteTensor do
  @moduledoc """
  A typed multi-dimensional array used in Tensorflow Lite.
  """

  alias TFLiteBEAM.TFLiteQuantizationParams

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
    :tflite_beam_nif.tflitetensor_type(self)
  end

  @doc """
  Get the dimensions (C++) API
  """
  @spec dims(%T{}) :: [integer()]
  def dims(%T{shape: shape}), do: Tuple.to_list(shape)

  @spec dims(reference()) :: [integer()] | nif_error()
  def dims(self) do
    with {:ok, dims} <- :tflite_beam_nif.tflitetensor_dims(self) do
      dims
    else
      error -> error
    end
  end

  @doc """
  Get the tensor shape
  """
  @spec shape(%T{}) :: tuple()
  def shape(%T{shape: shape}), do: shape

  @spec shape(reference()) :: tuple() | nif_error()
  def shape(self) do
    with {:ok, dims} <- :tflite_beam_nif.tflitetensor_dims(self) do
      List.to_tuple(dims)
    else
      error -> error
    end
  end

  @doc """
  Get the quantization params
  """
  @spec quantization_params(%T{} | reference()) :: %TFLiteQuantizationParams{} | nif_error()
  def quantization_params(%T{quantization_params: quantization_params}), do: quantization_params

  def quantization_params(self) do
    with {:ok, {scale, zero_point, quantized_dimension}} <-
           :tflite_beam_nif.tflitetensor_quantization_params(self) do
      %TFLiteQuantizationParams{
        scale: scale,
        zero_point: zero_point,
        quantized_dimension: quantized_dimension
      }
    else
      error -> error
    end
  end

  @doc """
  Set tensor data
  """
  @spec set_data(%T{} | reference(), binary() | %Nx.Tensor{}) :: :ok | nif_error()
  def set_data(%T{reference: reference}, data), do: set_data(reference, data)

  def set_data(self, %Nx.Tensor{} = data) when is_reference(self) do
    :tflite_beam_nif.tflitetensor_set_data(self, Nx.to_binary(data))
  end

  def set_data(self, data) when is_reference(self) and is_binary(data) do
    :tflite_beam_nif.tflitetensor_set_data(self, data)
  end

  @doc """
  Get binary data
  """
  @spec to_binary(%T{} | reference(), non_neg_integer()) :: binary() | {:error, String.t()}
  def to_binary(self, limit \\ 0)

  def to_binary(%T{reference: reference}, limit) when limit >= 0 do
    to_binary(reference, limit)
  end

  def to_binary(self, limit) when is_reference(self) and limit >= 0 do
    with {:ok, binary} <- :tflite_beam_nif.tflitetensor_to_binary(self, limit) do
      binary
    else
      error -> error
    end
  end

  @doc """
  Convert `TFLiteBEAM.TFLiteTensor` to `Nx.Tensor`
  """
  @spec to_nx(reference() | %T{}, Keyword.t()) :: %Nx.Tensor{}
  def to_nx(self_struct, opts \\ [])

  def to_nx(self_struct, opts) when is_struct(self_struct, T) and is_list(opts) do
    type = type(self_struct)
    shape = List.to_tuple(dims(self_struct))
    backend = opts[:backend]

    case to_binary(self_struct) do
      binary when is_binary(binary) ->
        to_nx_backend(binary, type, backend)
        |> Nx.reshape(shape)

      error ->
        error
    end
  end

  def to_nx(self, opts) when is_reference(self) and is_list(opts) do
    type = type(self)
    shape = List.to_tuple(dims(self))
    backend = opts[:backend]

    case to_binary(self) do
      binary when is_binary(binary) ->
        to_nx_backend(binary, type, backend)
        |> Nx.reshape(shape)

      error ->
        error
    end
  end

  defp to_nx_backend(binary, type, backend) do
    case backend do
      nil ->
        Nx.from_binary(binary, type)

      module when is_atom(module) ->
        if Code.ensure_loaded?(module) do
          Nx.from_binary(binary, type, backend: module)
        else
          raise "Expecting keyword parameter `backend` to be a module, however, got `#{inspect(module)}`"
        end

      error ->
        raise "Expecting keyword parameter `backend` to be a module, however, got `#{inspect(error)}`"
    end
  end
end