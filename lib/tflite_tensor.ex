defmodule TFLiteElixir.TfLiteTensor do
  import TFLiteElixir.Errorize

  @behaviour Nx.Backend
  alias Nx.Tensor, as: T

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

  alias __MODULE__, as: TB

  @doc """
  Get the data type
  """
  @spec type(%TB{}) :: tensor_type()
  def type(%TB{type: type}), do: type

  @spec type(reference()) :: tensor_type() | nif_error()
  def type(self) when is_reference(self) do
    TFLiteElixir.Nif.tflitetensor_type(self)
  end

  deferror(type(self))

  @doc """
  Get the dimensions
  """
  @spec dims(%TB{}) :: [integer()]
  def dims(%TB{shape: shape}), do: shape

  @spec dims(reference()) :: {:ok, [integer()]} | nif_error()
  def dims(self) do
    TFLiteElixir.Nif.tflitetensor_dims(self)
  end

  deferror(dims(self))

  @doc """
  Get the quantization params
  """
  def quantization_params(%TB{quantization_params: quantization_params}), do: quantization_params

  def quantization_params(self) do
    TFLiteElixir.Nif.tflitetensor_quantization_params(self)
  end

  deferror(quantization_params(self))

  @doc """
  Set tensor data
  """
  def set_data(%TB{reference: reference}, data), do: set_data(reference, data)

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

  @spec to_binary(%TB{}, non_neg_integer()) :: binary()
  def to_binary(%TB{reference: reference}, limit), do: to_binary!(reference, limit)

  @spec to_binary(reference(), non_neg_integer()) :: binary()
  def to_binary(self, limit) when is_reference(self) and limit >= 0 do
    TFLiteElixir.Nif.tflitetensor_to_binary(self, limit)
  end

  deferror(to_binary(self, limit))

  @impl true
  def to_binary(%T{data: %TB{reference: tensor_ref}} = _tensor, limit) when is_reference(tensor_ref) and is_integer(limit) and limit >= 0 do
    to_binary!(tensor_ref, limit)
  end

  @doc false
  def from_nx(%T{data: %TB{reference: tensor_ref}}), do: tensor_ref
  def from_nx(%T{} = _tensor) do
    raise "cannot allocate tensor"
  end

  @doc """
  To Nx.Tensor
  """
  @spec to_nx(%TB{}) :: binary()
  def to_nx(%TB{reference: tensor_ref} = _self) do
    to_nx(tensor_ref)
  end

  @spec to_nx(reference()) :: binary()
  def to_nx(self) when is_reference(self) do
    type = type!(self)
    shape = List.to_tuple(dims!(self))
    %T{
      type: type,
      shape: shape,
      names: [],
      data: %__MODULE__{reference: check_shape_and_type!(self, shape, type)}
    }
  end

  deferror(to_nx(self))

  def to_nx(%TB{}=self, backend) do
    Nx.from_binary(to_binary(self), type(self), backend: backend)
    |> Nx.reshape(List.to_tuple(dims!(self)))
  end

  @doc false
  def to_nx(tensor_ref, %T{type: _type, shape: shape} = t)
      when is_reference(tensor_ref) do
    type = type!(tensor_ref)
    %{t | type: type, data: %__MODULE__{reference: check_shape_and_type!(tensor_ref, shape, type)}}
  end

  if Application.compile_env(:tflite_elixir, :check_shape_and_type, false) do
    defp check_shape_and_type!(tensor_ref, shape, type) do
      current_type = type!(tensor_ref)

      if current_type != type do
        raise "type mismatch in TFLite: expected #{inspect(type)}, got: #{inspect(current_type)}. " <>
              "Please report this bug"
      end

      current_shape = List.to_tuple(dims!(tensor_ref))

      if current_shape != shape do
        raise "shape mismatch in TFLite: expected #{inspect(shape)}, got: #{inspect(current_shape)}. " <>
              "Please report this bug"
      end

      tensor_ref
    end
  else
    defp check_shape_and_type!(tensor_ref, _, _), do: tensor_ref
  end

  @impl true
  def inspect(%T{data: %TB{reference: tensor_ref}} = tensor, inspect_opts) do
    _limit = if inspect_opts.limit == :infinity, do: :infinity, else: inspect_opts.limit + 1

    tensor_ref
    |> to_binary!(0)
    |> then(&Nx.Backend.inspect(tensor, &1, inspect_opts))
    |> maybe_add_signature(tensor)
  end

  if Application.compile_env(:tflite_elixir, :add_backend_on_inspect, true) do
    defp maybe_add_signature(result, %T{data: %TB{reference: _tensor_ref}}) do
      Inspect.Algebra.concat([
        "TFLite.Backend",
        Inspect.Algebra.line(),
        result
      ])
    end
  else
    defp maybe_add_signature(result, _tensor) do
      result
    end
  end
end
