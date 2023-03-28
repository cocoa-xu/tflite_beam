defmodule TFLiteBEAM do
  @moduledoc """
  This module contains some helper functions from the `tflite`
  namespace in TensorFlow Lite's codebase.
  """

  alias TFLiteBEAM.TFLiteTensor

  @doc """
  Prints a dump of what tensors and what nodes are in the interpreter.

  Note that this function directly prints to stdout
  """
  @spec print_interpreter_state(reference()) :: nil
  def print_interpreter_state(interpreter) do
    :tflite_beam_nif.tflite_print_interpreter_state(interpreter)
    nil
  end

  @doc """
  Resets a variable tensor to the default value.
  """
  @spec reset_variable_tensor(%TFLiteTensor{}) :: any
  def reset_variable_tensor(%TFLiteTensor{} = tflite_tensor) do
    :tflite_beam_nif.tflite_reset_variable_tensor(tflite_tensor)
  end
end
