defmodule TFLite do
  @spec printInterpreterState(reference()) :: nil
  def printInterpreterState(interpreter) do
    TFLite.Nif.tflite_printInterpreterState(interpreter)
    nil
  end
end
