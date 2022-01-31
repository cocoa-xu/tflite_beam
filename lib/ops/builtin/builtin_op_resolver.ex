defmodule TFLite.Ops.Builtin.BuiltinResolver do
  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}

  @spec new() :: nif_resource_ok() | nif_error()
  def new() do
    TFLite.Nif.ops_builtin_builtinResolver_new()
  end

  @spec new!() :: reference()
  def new!() do
    {:ok, resolver} = new()
    resolver
  end
end
