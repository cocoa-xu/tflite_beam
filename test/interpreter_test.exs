defmodule TFLiteElixir.Interpreter.Test do
  use ExUnit.Case

  alias TFLiteElixir.InterpreterBuilder
  alias TFLiteElixir.Ops.Builtin.BuiltinResolver
  alias TFLiteElixir.FlatBufferModel
  alias TFLiteElixir.Interpreter
  alias TFLiteElixir.TFLiteTensor
  alias TFLiteElixir.TFLiteQuantizationParams

  test "set_inputs/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)

    assert {:ok, [0]} == Interpreter.inputs(interpreter)
    assert :ok == Interpreter.set_inputs(interpreter, [1])
    assert {:ok, [1]} == Interpreter.inputs(interpreter)
    assert :ok == Interpreter.set_inputs(interpreter, [0])
    assert {:ok, [0]} == Interpreter.inputs(interpreter)
  end

  test "set_outputs/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)

    assert {:ok, [171]} == Interpreter.outputs(interpreter)
    assert :ok == Interpreter.set_outputs(interpreter, [170])
    assert {:ok, [170]} == Interpreter.outputs(interpreter)
    assert :ok == Interpreter.set_outputs(interpreter, [171])
    assert {:ok, [171]} == Interpreter.outputs(interpreter)
  end

  test "set_variables/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)

    assert {:ok, []} == Interpreter.variables(interpreter)
    assert :ok == Interpreter.set_variables(interpreter, [1, 2])
    assert {:ok, [1, 2]} == Interpreter.variables(interpreter)
    assert :ok == Interpreter.set_variables(interpreter, [])
    assert {:ok, []} == Interpreter.variables(interpreter)
  end

  test "inputs/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert {:ok, [0]} == Interpreter.inputs(interpreter)
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

  test "outputs/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert {:ok, [171]} == Interpreter.outputs(interpreter)
  end

  test "variables/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)

    assert {:ok, []} == Interpreter.variables(interpreter)
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

  test "tensors_size/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)

    assert 179 == Interpreter.tensors_size(interpreter)
  end

  test "nodes_size/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)

    assert 65 == Interpreter.nodes_size(interpreter)
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

  test "tensor/2 with out-of-bound index" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)

    assert {:error, "index out of bound"} == Interpreter.tensor(interpreter, 100000)
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

  test "predict/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    expected_out = Path.join([__DIR__, "test_data", "parrot-expected-out.bin"]) |> File.read!()
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    :ok = InterpreterBuilder.build!(builder, interpreter)
    :ok = Interpreter.allocate_tensors!(interpreter)

    input_tensor = Nx.from_binary(input_data, :f32)
    assert {:error, "input data type, {:f, 32}, does not match the data type of the tensor, {:u, 8}, tensor index: 0"} == Interpreter.predict(interpreter, input_tensor)

    input_tensor = Nx.from_binary(input_data, :u8)
    assert {:error, "input data shape, {150528}, does not match the shape type of the tensor, {1, 224, 224, 3}, tensor index: 0"} == Interpreter.predict(interpreter, input_tensor)

    input_tensor = Nx.reshape(input_tensor, {1, 224, 224, 3})
    [output_data] = Interpreter.predict(interpreter, input_tensor)
    assert expected_out == Nx.to_binary(output_data)

    [output_data] = Interpreter.predict(interpreter, [input_tensor])
    assert expected_out == Nx.to_binary(output_data)

    error = Interpreter.predict(interpreter, [input_tensor, input_tensor])
    assert {:error, "length mismatch: there are 1 input tensors while the input list has 2 elements"} == error

    error = Interpreter.predict(interpreter, [Nx.from_binary(input_data, :f32)])
    assert [error: "input data type, {:f, 32}, does not match the data type of the tensor, {:u, 8}, tensor index: 0"] == error

    error = Interpreter.predict(interpreter, %{"A" => input_tensor})
    assert {:error, "missing input data for tensor `map/TensorArrayStack/TensorArrayGatherV3`, tensor index: 0"} == error

    [output_data] = Interpreter.predict(interpreter, %{"map/TensorArrayStack/TensorArrayGatherV3" => input_tensor})
    assert expected_out == Nx.to_binary(output_data)

    [output_data] = Interpreter.predict(interpreter, [input_data])
    assert expected_out == Nx.to_binary(output_data)

    [output_data] = Interpreter.predict(interpreter, %{"map/TensorArrayStack/TensorArrayGatherV3" => input_data})
    assert expected_out == Nx.to_binary(output_data)
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

  test "TFLiteElixir.Interpreter.new(model_path)" do
    model_path = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    _interpreter = TFLiteElixir.Interpreter.new!(model_path)

    {error_at_stage, {:error, reason}} = TFLiteElixir.Interpreter.new("/dev/null")
    assert :build_from_file == error_at_stage
    assert reason == "cannot get flatbuffer model"
  end
end
