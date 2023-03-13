defmodule TFLiteElixir.Ops.Builtin.BuiltinResolver do
  import TFLiteElixir.Errorize

  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}

  @doc """
  New built-in op resolver.

  This resolver provides a list of TfLite delegates that could be
  applied by TfLite interpreter by default.
  """
  @spec new() :: nif_resource_ok() | nif_error()
  def new() do
    TFLiteElixir.Nif.ops_builtin_builtinResolver_new()
  end

  deferror(new())
end
