defmodule TFLiteElixir.InterpreterBuilder.Test do
  use ExUnit.Case

  alias TFLiteElixir.InterpreterBuilder
  alias TFLiteElixir.Ops.Builtin.BuiltinResolver
  alias TFLiteElixir.FlatBufferModel
  alias TFLiteElixir.Interpreter
  alias TFLiteElixir.TFLiteTensor
  alias TFLiteElixir.TFLiteQuantizationParams

  test "inputs/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert {:ok, [0]} == Interpreter.inputs(interpreter)
  end

  test "outputs/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert {:ok, [171]} == Interpreter.outputs(interpreter)
  end

  test "get_input_name/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert {:ok, "map/TensorArrayStack/TensorArrayGatherV3"} ==
             Interpreter.get_input_name(interpreter, 0)
  end

  test "get_output_name/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert {:ok, "prediction"} == Interpreter.get_output_name(interpreter, 0)
  end

  test "tensor/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    %TFLiteTensor{
      name: "map/TensorArrayStack/TensorArrayGatherV3",
      index: 0,
      shape: [1, 224, 224, 3],
      shape_signature: [1, 224, 224, 3],
      type: {:u, 8},
      quantization_params: %TFLiteQuantizationParams{
        scale: [0.0078125],
        zero_point: [128],
        quantized_dimension: 0
      },
      sparsity_params: %{}
    } = Interpreter.tensor(interpreter, 0)
  end

  test "allocate_tensors/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert :ok == Interpreter.allocate_tensors!(interpreter)
  end

  test "input_tensor/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert :ok == Interpreter.allocate_tensors!(interpreter)
    assert :ok == Interpreter.input_tensor!(interpreter, 0, input_data)
  end

  test "invoke/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert :ok == Interpreter.allocate_tensors!(interpreter)
    assert :ok == Interpreter.input_tensor!(interpreter, 0, input_data)
    assert :ok == Interpreter.invoke!(interpreter)
  end

  test "output_tensor/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    expected_out = Path.join([__DIR__, "test_data", "parrot-expected-out.bin"]) |> File.read!()
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert :ok == Interpreter.allocate_tensors!(interpreter)
    assert :ok == Interpreter.input_tensor!(interpreter, 0, input_data)
    assert :ok == Interpreter.invoke!(interpreter)

    output_data = Interpreter.output_tensor!(interpreter, 0)
    assert expected_out == output_data
  end
end
