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
    :ok = TFLite.InterpreterBuilder.build(builder, interpreter)
    {:ok, [0]} = TFLite.Interpreter.inputs(interpreter)
    {:ok, [171]} = TFLite.Interpreter.outputs(interpreter)
    {:ok, "map/TensorArrayStack/TensorArrayGatherV3"} = TFLite.Interpreter.getInputName(interpreter, 0)
    {:ok, "prediction"} = TFLite.Interpreter.getOutputName(interpreter, 0)
    {:ok, input_tensor} = TFLite.Interpreter.tensor(interpreter, 0)
    {:ok, [1, 224, 224, 3]} = TFLite.TfLiteTensor.dims(input_tensor)
    {:u, 8} = TFLite.TfLiteTensor.type(input_tensor)
    {:ok, output_tensor} = TFLite.Interpreter.tensor(interpreter, 171)
    {:ok, [1, 965]} = TFLite.TfLiteTensor.dims(output_tensor)
    {:u, 8} = TFLite.TfLiteTensor.type(output_tensor)
    interpreter
  end
end
