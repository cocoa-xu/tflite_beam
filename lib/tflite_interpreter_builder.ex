defmodule TFLite.InterpreterBuilder do
  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}

  @spec new(reference(), reference()) :: nif_resource_ok() | nif_error()
  def new(model, resolver) when is_reference(model) and is_reference(resolver) do
    TFLite.Nif.interpreterBuilder_new(model, resolver)
  end

  @spec new!(reference(), reference()) :: reference()
  def new!(model, resolver) when is_reference(model) and is_reference(resolver) do
    TFLite.Nif.interpreterBuilder_new(model, resolver)
  end

  @spec build(reference(), reference()) :: :ok | nif_error()
  def build(self, interpreter) do
    TFLite.Nif.interpreter_build(self, interpreter)
  end

  @spec build!(reference(), reference()) :: :ok
  def build!(self, interpreter) do
    :ok = build(self, interpreter)
    :ok
  end
end
