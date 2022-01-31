defmodule TFLite.InterpreterBuilder do
  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}

  @doc """
  New InterpreterBuilder
  """
  @spec new(reference(), reference()) :: nif_resource_ok() | nif_error()
  def new(model, resolver) when is_reference(model) and is_reference(resolver) do
    TFLite.Nif.interpreterBuilder_new(model, resolver)
  end

  @doc """
  New InterpreterBuilder
  """
  @spec new!(reference(), reference()) :: reference()
  def new!(model, resolver) when is_reference(model) and is_reference(resolver) do
    {:ok, builder} = new(model, resolver)
    builder
  end

  @doc """
  Build the interpreter with the InterpreterBuilder.

  Note: all Interpreters should be built with the InterpreterBuilder,
  which allocates memory for the Interpreter and does various set up
  tasks so that the Interpreter can read the provided model.
  """
  @spec build(reference(), reference()) :: :ok | nif_error()
  def build(self, interpreter) do
    TFLite.Nif.interpreterBuilder_build(self, interpreter)
  end

  @doc """
  Build the interpreter with the InterpreterBuilder.

  Note: all Interpreters should be built with the InterpreterBuilder,
  which allocates memory for the Interpreter and does various set up
  tasks so that the Interpreter can read the provided model.
  """
  @spec build!(reference(), reference()) :: :ok
  def build!(self, interpreter) do
    :ok = build(self, interpreter)
    :ok
  end
end
