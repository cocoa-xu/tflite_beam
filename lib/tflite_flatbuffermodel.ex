defmodule TFLite.FlatBufferModel do
  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}

  @doc """
  Build model from a given tflite file

  Note that if the tensorflow-lite library was compiled with `TFLITE_MCU`,
  then this function will always have return type `nif_error()`
  """
  @spec buildFromFile(String.t()) :: nif_resource_ok() | nif_error()
  def buildFromFile(filename) when is_binary(filename) do
    TFLite.Nif.flatBufferModel_buildFromFile(filename)
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
  @spec buildFromBuffer(binary()) :: nif_resource_ok() | nif_error()
  def buildFromBuffer(buffer) when is_binary(buffer) do
    TFLite.Nif.flatBufferModel_buildFromBuffer(buffer)
  end

  @spec initialized(reference()) :: bool() | nif_error()
  def initialized(self) when is_reference(self) do
    TFLite.Nif.flatBufferModel_initialized(self)
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
  @spec getMinimumRuntime(reference()) :: String.t() | nif_error()
  def getMinimumRuntime(self) when is_reference(self) do
    TFLite.Nif.flatBufferModel_getMinimumRuntime(self)
  end

  @doc """
  Return model metadata as a mapping of name & buffer strings.

  See Metadata table in TFLite schema.
  """
  @spec readAllMetadata(reference()) :: %{String.t() => String.t()} | nif_error()
  def readAllMetadata(self) when is_reference(self) do
    TFLite.Nif.flatBufferModel_readAllMetadata(self)
  end
end
