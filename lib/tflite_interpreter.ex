defmodule TFLite.Interpreter do
  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}

  @spec new() :: nif_resource_ok() | nif_error()
  def new() do
    TFLite.Nif.interpreter_new()
  end

  @spec new!() :: reference()
  def new!() do
    {:ok, interpreter} = new()
    interpreter
  end

  def allocateTensors(self) do
    TFLite.Nif.interpreter_allocateTensors(self)
  end
end
