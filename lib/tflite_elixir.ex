defmodule TFLiteElixir do
  alias TFLiteElixir.TFLiteTensor

  @doc """
  Prints a dump of what tensors and what nodes are in the interpreter.

  Note that this function directly prints to stdout
  """
  @spec print_interpreter_state(reference()) :: nil
  def print_interpreter_state(interpreter) do
    TFLiteElixir.Nif.tflite_printInterpreterState(interpreter)
    nil
  end

  def reset_variable_tensor(%TFLiteTensor{}=tflite_tensor) do
    TFLiteElixir.Nif.tflite_resetVariableTensor(tflite_tensor)
  end
end
