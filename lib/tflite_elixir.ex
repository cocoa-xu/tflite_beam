defmodule TFLiteElixir do
  alias TFLiteElixir.TFLiteTensor

  @doc """
  Prints a dump of what tensors and what nodes are in the interpreter.

  Note that this function directly prints to stdout
  """
  @spec printInterpreterState(reference()) :: nil
  def printInterpreterState(interpreter) do
    TFLiteElixir.Nif.tflite_printInterpreterState(interpreter)
    nil
  end

  @spec resetVariableTensor(%TFLiteElixir.TFLiteTensor{optional(any) => any}) :: any
  def resetVariableTensor(%TFLiteTensor{}=tflite_tensor) do
    TFLiteElixir.Nif.tflite_resetVariableTensor(tflite_tensor)
  end
end
