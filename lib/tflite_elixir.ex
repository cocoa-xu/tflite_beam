defmodule TFLiteElixir do
  @doc """
  Prints a dump of what tensors and what nodes are in the interpreter.

  Note that this function directly prints to stdout
  """
  @spec printInterpreterState(reference()) :: nil
  def printInterpreterState(interpreter) do
    TFLiteElixir.Nif.tflite_printInterpreterState(interpreter)
    nil
  end
end
