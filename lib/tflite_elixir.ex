defmodule TFLite do
  @spec printInterpreterState(reference()) :: nil
  def printInterpreterState(interpreter) do
    TFLite.Nif.tflite_printInterpreterState(interpreter)
  end

  def test() do
    filename = "mobilenet_v2_1.0_224_inat_bird_quant.tflite"
    {:ok, model} = TFLite.FlatBufferModel.buildFromFile(filename)
    {:ok, resolver} = TFLite.Ops.Builtin.BuiltinResolver.new()
    {:ok, builder} = TFLite.InterpreterBuilder.new(model, resolver)
    {:ok, interpreter} = TFLite.Interpreter.new()
    :ok = TFLite.Interpreter.build(interpreter, builder)
  end
end
