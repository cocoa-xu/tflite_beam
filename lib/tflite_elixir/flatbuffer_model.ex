defmodule TFLiteElixir.FlatBufferModel do
  @moduledoc """
  An RAII object that represents a read-only tflite model, copied from disk, or
  mmapped.
  """
  import TFLiteElixir.Errorize

  alias TFLiteElixir.ErrorReporter

  @type nif_error :: {:error, String.t()}

  defstruct [
    :initialized,
    :minimum_runtime,
    :model
  ]

  alias __MODULE__, as: T

  @doc """
  Build model from a given tflite file

  Note that if the tensorflow-lite library was compiled with `TFLITE_MCU`,
  then this function will always have return type `nif_error()`

  ##### Keyword parameters
  - `error_reporter`: `TFLiteElixir.ErrorReporter`.

    Caller retains ownership of `error_reporter` and must ensure its lifetime
    is longer than the FlatBufferModel instance.
  """
  @spec build_from_file(String.t()) :: %T{} | nif_error()
  def build_from_file(filename, opts \\ []) when is_binary(filename) and is_list(opts) do
    error_reporter = ErrorReporter.from_struct(opts[:error_reporter])

    with {:ok, model} <-
           TFLiteElixir.Nif.flatbuffer_model_build_from_file(filename, error_reporter) do
      %T{
        initialized: TFLiteElixir.Nif.flatbuffer_model_initialized(model),
        minimum_runtime: TFLiteElixir.Nif.flatbuffer_model_get_minimum_runtime(model),
        model: model
      }
    else
      error -> error
    end
  end

  deferror(build_from_file(filename, opts))

  @doc """
  Verifies whether the content of the file is legit, then builds a model
  based on the file.

  ##### Keyword parameters
  - `error_reporter`: `TFLiteElixir.ErrorReporter`.

    Caller retains ownership of `error_reporter` and must ensure its lifetime
    is longer than the FlatBufferModel instance.

  Returns `:invalid` in case of failure.
  """
  @spec verify_and_build_from_file(String.t(), Keyword.t()) ::
          %T{} | :invalid | {:error, String.t()}
  def verify_and_build_from_file(filename, opts \\ []) do
    error_reporter = ErrorReporter.from_struct(opts[:error_reporter])

    with {:ok, model} <-
           TFLiteElixir.Nif.flatbuffer_model_verify_and_build_from_file(filename, error_reporter) do
      %T{model: model}
    else
      error -> error
    end
  end

  @doc """
  Build model from caller owned memory buffer

  Note that `buffer` will NOT be copied. Caller has the ensure that
  the buffer lives longer than the returned `reference` of `TFLite.FlatBufferModel`

  Discussion:

    We can copy the data in the NIF, but `FlatBufferModel::BuildFromBuffer` always
    assumes that the buffer is owner by the caller, (in this case, the binding code)

    However, we would have no way to release the copied memory because we couldn't
    identify if the `allocation_` borrows or owns that memory.
  """
  @spec build_from_buffer(binary(), Keyword.t()) :: %T{} | nif_error()
  def build_from_buffer(buffer, opts \\ []) when is_binary(buffer) and is_list(opts) do
    error_reporter = ErrorReporter.from_struct(opts[:error_reporter])

    with {:ok, model} <-
           TFLiteElixir.Nif.flatbuffer_model_build_from_buffer(buffer, error_reporter) do
      %T{model: model}
    else
      error -> error
    end
  end

  @doc """
  Check whether current model has been initialized
  """
  @spec initialized(%T{}) :: bool() | nif_error()
  def initialized(%T{model: self}) when is_reference(self) do
    TFLiteElixir.Nif.flatbuffer_model_initialized(self)
  end

  @spec error_reporter(%T{:model => reference()}) :: %ErrorReporter{} | {:error, String.t()}
  def error_reporter(%T{model: self}) when is_reference(self) do
    case TFLiteElixir.Nif.flatbuffer_model_error_reporter(self) do
      {:ok, ref} when is_reference(ref) -> %ErrorReporter{ref: ref}
      error -> error
    end
  end

  @doc """
  Returns the minimum runtime version from the flatbuffer. This runtime
  version encodes the minimum required interpreter version to run the
  flatbuffer model. If the minimum version can't be determined, an empty
  string will be returned.

  Note that the returned minimum version is a lower-bound but not a strict
  lower-bound; ops in the graph may not have an associated runtime version,
  in which case the actual required runtime might be greater than the
  reported minimum.
  """
  @spec get_minimum_runtime(%T{}) :: String.t() | nif_error()
  def get_minimum_runtime(%T{model: self}) when is_reference(self) do
    TFLiteElixir.Nif.flatbuffer_model_get_minimum_runtime(self)
  end

  @doc """
  Return model metadata as a mapping of name & buffer strings.

  See Metadata table in TFLite schema.
  """
  @spec read_all_metadata(%T{}) :: %{String.t() => String.t()} | nif_error()
  def read_all_metadata(%T{model: self}) when is_reference(self) do
    TFLiteElixir.Nif.flatbuffer_model_read_all_metadata(self)
  end

  defimpl Inspect, for: T do
    import Inspect.Algebra

    def inspect(self, opts) do
      concat([
        "#FlatBufferModel<",
        to_doc(
          %{
            :initialized => T.initialized(self),
            :minimum_runtime => T.get_minimum_runtime(self)
          },
          opts
        ),
        ">"
      ])
    end
  end
end
