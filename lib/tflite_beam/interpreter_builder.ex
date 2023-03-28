defmodule TFLiteBEAM.InterpreterBuilder do
  @moduledoc """
  Build an interpreter capable of interpreting model.
  """
  import TFLiteBEAM.Errorize
  alias TFLiteBEAM.FlatBufferModel

  @type nif_resource_ok :: {:ok, reference()}
  @type nif_error :: {:error, String.t()}

  @doc """
  New InterpreterBuilder
  """
  @spec new(%FlatBufferModel{}, reference()) :: nif_resource_ok() | nif_error()
  def new(%FlatBufferModel{model: model}, resolver) when is_reference(resolver) do
    :tflite_beam_interpreter_builder.new(model, resolver)
  end

  deferror(new(model, resolver))

  @doc """
  Build the interpreter with the InterpreterBuilder.

  Note: all Interpreters should be built with the InterpreterBuilder,
  which allocates memory for the Interpreter and does various set up
  tasks so that the Interpreter can read the provided model.
  """
  @spec build(reference(), reference()) :: :ok | nif_error()
  def build(self, interpreter) do
    :tflite_beam_interpreter_builder.build(self, interpreter)
  end

  deferror(build(self, interpreter))

  @doc """
  Sets the number of CPU threads to use for the interpreter.
  Returns `true` on success, `{:error, reason}` on error.
  """
  @spec set_num_threads(reference(), integer()) :: :ok | nif_error()
  def set_num_threads(self, num_threads) when is_integer(num_threads) and num_threads >= 1 do
    :tflite_beam_interpreter_builder.set_num_threads(self, num_threads)
  end

  deferror(set_num_threads(self, num_threads))
end
