defmodule TFLiteElixir.Test do
  use ExUnit.Case

  def verify_loaded_model(model, input_data, expected_out, print_state)
      when is_binary(input_data) and is_binary(expected_out) and
             is_boolean(print_state) do
    # build interpreter
    %{"TFLITE_METADATA" => <<28>>, "min_runtime_version" => "1.5.0"} =
      TFLiteElixir.FlatBufferModel.readAllMetadata!(model)

    true = TFLiteElixir.FlatBufferModel.initialized!(model)
    "1.5.0" = TFLiteElixir.FlatBufferModel.getMinimumRuntime!(model)
    resolver = TFLiteElixir.Ops.Builtin.BuiltinResolver.new!()
    builder = TFLiteElixir.InterpreterBuilder.new!(model, resolver)
    interpreter = TFLiteElixir.Interpreter.new!()
    TFLiteElixir.InterpreterBuilder.setNumThreads!(builder, 2)
    :ok = TFLiteElixir.InterpreterBuilder.build!(builder, interpreter)
    TFLiteElixir.Interpreter.setNumThreads!(interpreter, 2)

    # verify
    [0] = TFLiteElixir.Interpreter.inputs!(interpreter)
    [171] = TFLiteElixir.Interpreter.outputs!(interpreter)


    "map/TensorArrayStack/TensorArrayGatherV3" =
      TFLiteElixir.Interpreter.getInputName!(interpreter, 0)

    "prediction" = TFLiteElixir.Interpreter.getOutputName!(interpreter, 0)
    input_tensor = %TFLiteElixir.TfLiteTensor{
      name: "map/TensorArrayStack/TensorArrayGatherV3",
      index: 0,
      shape: [1, 224, 224, 3],
      shape_signature: [1, 224, 224, 3],
      type: {:u, 8},
      quantization_params: %TFLiteElixir.TFLiteQuantizationParams{
        scale: [0.0078125],
        zero_point: [128],
        quantized_dimension: 0
      },
      sparsity_params: %{},
    } = TFLiteElixir.Interpreter.tensor!(interpreter, 0)
    [1, 224, 224, 3] = TFLiteElixir.TfLiteTensor.dims!(input_tensor)
    {:u, 8} = TFLiteElixir.TfLiteTensor.type(input_tensor)
    output_tensor = TFLiteElixir.Interpreter.tensor!(interpreter, 171)
    [1, 965] = TFLiteElixir.TfLiteTensor.dims!(output_tensor)
    {:u, 8} = TFLiteElixir.TfLiteTensor.type!(output_tensor)

    # run forwarding
    :ok = TFLiteElixir.Interpreter.allocateTensors!(interpreter)
    TFLiteElixir.Interpreter.input_tensor!(interpreter, 0, input_data)
    TFLiteElixir.Interpreter.invoke!(interpreter)
    output_data = TFLiteElixir.Interpreter.output_tensor!(interpreter, 0)
    true = expected_out == output_data

    if print_state, do: TFLiteElixir.printInterpreterState(interpreter)
    :ok
  end

  test "mobilenet_v2_1.0_224_inat_bird_quant buildFromFile" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    expected_out = Path.join([__DIR__, "test_data", "parrot-expected-out.bin"]) |> File.read!()
    model = TFLiteElixir.FlatBufferModel.buildFromFile!(filename)
    :ok = verify_loaded_model(model, input_data, expected_out, true)
  end

  test "TFLite.Interpreter.new(model_path)" do
    model_path = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    _interpreter = TFLiteElixir.Interpreter.new!(model_path)

    {error_at_stage, {:error, reason}} = TFLiteElixir.Interpreter.new("/dev/null")
    assert :build_from_file == error_at_stage
    assert reason == "cannot load flat buffer model from file"
  end

  test "mobilenet_v2_1.0_224_inat_bird_quant buildFromBuffer" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    expected_out = Path.join([__DIR__, "test_data", "parrot-expected-out.bin"]) |> File.read!()
    model = TFLiteElixir.FlatBufferModel.buildFromBuffer!(File.read!(filename))
    :ok = verify_loaded_model(model, input_data, expected_out, false)
  end

  with {:module, TFLiteElixir.Coral} <- Code.ensure_compiled(TFLiteElixir.Coral) do
    test "Contains EdgeTpu Custom Op" do
      filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
      model = TFLiteElixir.FlatBufferModel.buildFromBuffer!(File.read!(filename))
      false = TFLiteElixir.Coral.containsEdgeTpuCustomOp?(model)

      filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant_edgetpu.tflite"])
      model = TFLiteElixir.FlatBufferModel.buildFromBuffer!(File.read!(filename))
      true = TFLiteElixir.Coral.containsEdgeTpuCustomOp?(model)
    end
  end
end
