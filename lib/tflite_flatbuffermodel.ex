defmodule TFLite.FlatBufferModel do
  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}

  @spec buildFromFile(String.t()) :: nif_resource_ok() | nif_error()
  def buildFromFile(filename) when is_binary(filename) do
    TFLite.Nif.flatBufferModel_buildFromFile(filename)
  end
end
