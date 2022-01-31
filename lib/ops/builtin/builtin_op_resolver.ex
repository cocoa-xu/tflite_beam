defmodule TFLite.Ops.Builtin.BuiltinResolver do
  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}

  @doc """
  New built-in op resolver.

  This resolver provides a list of TfLite delegates that could be
  applied by TfLite interpreter by default.
  """
  @spec new() :: nif_resource_ok() | nif_error()
  def new() do
    TFLite.Nif.ops_builtin_builtinResolver_new()
  end

  @doc """
  New built-in op resolver.

  This resolver provides a list of TfLite delegates that could be
  applied by TfLite interpreter by default.
  """
  @spec new!() :: reference()
  def new!() do
    {:ok, resolver} = new()
    resolver
  end
end
