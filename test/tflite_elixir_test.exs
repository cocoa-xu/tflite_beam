defmodule TfliteElixirTest do
  use ExUnit.Case

  def verify_loaded_model(model, input_data, expected_out, print_state)
      when is_reference(model) and is_binary(input_data) and is_binary(expected_out) and
             is_boolean(print_state) do
    # build interpreter
    %{"TFLITE_METADATA" => <<28>>, "min_runtime_version" => "1.5.0"} =
      TFLite.FlatBufferModel.readAllMetadata(model)

    true = TFLite.FlatBufferModel.initialized(model)
    "1.5.0" = TFLite.FlatBufferModel.getMinimumRuntime(model)
    resolver = TFLite.Ops.Builtin.BuiltinResolver.new!()
    builder = TFLite.InterpreterBuilder.new!(model, resolver)
    interpreter = TFLite.Interpreter.new!()
    :ok = TFLite.InterpreterBuilder.build!(builder, interpreter)

    # verify
    {:ok, [0]} = TFLite.Interpreter.inputs(interpreter)
    {:ok, [171]} = TFLite.Interpreter.outputs(interpreter)

    {:ok, "map/TensorArrayStack/TensorArrayGatherV3"} =
      TFLite.Interpreter.getInputName(interpreter, 0)

    {:ok, "prediction"} = TFLite.Interpreter.getOutputName(interpreter, 0)
    {:ok, input_tensor} = TFLite.Interpreter.tensor(interpreter, 0)
    {:ok, [1, 224, 224, 3]} = TFLite.TfLiteTensor.dims(input_tensor)
    {:u, 8} = TFLite.TfLiteTensor.type(input_tensor)
    {:ok, output_tensor} = TFLite.Interpreter.tensor(interpreter, 171)
    {:ok, [1, 965]} = TFLite.TfLiteTensor.dims(output_tensor)
    {:u, 8} = TFLite.TfLiteTensor.type(output_tensor)

    # run forwarding
    :ok = TFLite.Interpreter.allocateTensors(interpreter)
    TFLite.Interpreter.input_tensor(interpreter, 0, input_data)
    TFLite.Interpreter.invoke(interpreter)
    {:ok, output_data} = TFLite.Interpreter.output_tensor(interpreter, 0)
    true = expected_out == output_data

    if print_state, do: TFLite.printInterpreterState(interpreter)
    :ok
  end

  test "mobilenet_v2_1.0_224_inat_bird_quant buildFromFile" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    expected_out = Path.join([__DIR__, "test_data", "parrot-expected-out.bin"]) |> File.read!()
    {:ok, model} = TFLite.FlatBufferModel.buildFromFile(filename)
    :ok = verify_loaded_model(model, input_data, expected_out, true)
  end

  test "TFLite.Interpreter.new(model_path)" do
    model_path = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = TFLite.Interpreter.new!(model_path)
    assert is_reference(interpreter)

    {error_at_stage, {:error, reason}} = TFLite.Interpreter.new("/dev/null")
    assert :build_from_file == error_at_stage
    assert reason == "cannot load flat buffer model from file"
  end

  test "mobilenet_v2_1.0_224_inat_bird_quant buildFromBuffer" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    expected_out = Path.join([__DIR__, "test_data", "parrot-expected-out.bin"]) |> File.read!()
    {:ok, model} = TFLite.FlatBufferModel.buildFromBuffer(File.read!(filename))
    :ok = verify_loaded_model(model, input_data, expected_out, false)
  end
end
