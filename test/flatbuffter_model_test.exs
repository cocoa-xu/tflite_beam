defmodule TFLiteElixir.FlatBufferModel.Test do
  use ExUnit.Case, async: false

  alias TFLiteElixir.FlatBufferModel
  alias TFLiteElixir.ErrorReporter
  alias TFLiteElixir.InterpreterBuilder
  alias TFLiteElixir.Interpreter
  alias TFLiteElixir.Ops.Builtin.BuiltinResolver
  alias TFLiteElixir.TFLiteTensor
  alias TFLiteElixir.TFLiteQuantizationParams

  test "buildFromFile/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    expected_out = Path.join([__DIR__, "test_data", "parrot-expected-out.bin"]) |> File.read!()
    model = FlatBufferModel.build_from_file(filename)
    :ok = verify_loaded_model(model, input_data, expected_out, true)
  end

  test "buildFromFile/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    expected_out = Path.join([__DIR__, "test_data", "parrot-expected-out.bin"]) |> File.read!()

    model = FlatBufferModel.build_from_file(filename, error_reporter: ErrorReporter.default_error_reporter())

    :ok = verify_loaded_model(model, input_data, expected_out, true)
  end

  test "buildFromBuffer/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    expected_out = Path.join([__DIR__, "test_data", "parrot-expected-out.bin"]) |> File.read!()
    model = FlatBufferModel.build_from_buffer(File.read!(filename))
    :ok = verify_loaded_model(model, input_data, expected_out, false)
  end

  test "buildFromBuffer/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    input_data = Path.join([__DIR__, "test_data", "parrot.bin"]) |> File.read!()
    expected_out = Path.join([__DIR__, "test_data", "parrot-expected-out.bin"]) |> File.read!()
    model = FlatBufferModel.build_from_buffer(File.read!(filename), error_reporter: ErrorReporter.default_error_reporter())
    :ok = verify_loaded_model(model, input_data, expected_out, false)
  end

  test "initialized/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_buffer(File.read!(filename))
    assert true == FlatBufferModel.initialized(model)
  end

  test "get_minimum_runtime/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_buffer(File.read!(filename))
    assert true == FlatBufferModel.initialized(model)
    assert "1.5.0" == FlatBufferModel.get_minimum_runtime(model)
  end

  test "read_all_metadata/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_buffer(File.read!(filename))
    assert true == FlatBufferModel.initialized(model)
    assert %{"TFLITE_METADATA" => <<28>>, "min_runtime_version" => "1.5.0"} == FlatBufferModel.read_all_metadata!(model)
  end

  def verify_loaded_model(model, input_data, expected_out, print_state)
      when is_binary(input_data) and is_binary(expected_out) and
             is_boolean(print_state) do
    # build interpreter
    %{"TFLITE_METADATA" => <<28>>, "min_runtime_version" => "1.5.0"} = FlatBufferModel.read_all_metadata!(model)

    true = FlatBufferModel.initialized!(model)
    "1.5.0" = FlatBufferModel.get_minimum_runtime!(model)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()
    InterpreterBuilder.set_num_threads!(builder, 2)
    :ok = InterpreterBuilder.build!(builder, interpreter)
    Interpreter.set_num_threads!(interpreter, 2)

    # verify
    [0] = Interpreter.inputs!(interpreter)
    [171] = Interpreter.outputs!(interpreter)

    "map/TensorArrayStack/TensorArrayGatherV3" = Interpreter.get_input_name!(interpreter, 0)

    "prediction" = Interpreter.get_output_name!(interpreter, 0)

    input_tensor =
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

    [1, 224, 224, 3] = TFLiteTensor.dims!(input_tensor)
    {:u, 8} = TFLiteTensor.type(input_tensor)
    %TFLiteTensor{} = output_tensor = Interpreter.tensor(interpreter, 171)
    [1, 965] = TFLiteTensor.dims!(output_tensor)
    {:u, 8} = TFLiteTensor.type!(output_tensor)

    # run forwarding
    :ok = Interpreter.allocate_tensors!(interpreter)
    Interpreter.input_tensor!(interpreter, 0, input_data)
    Interpreter.invoke!(interpreter)
    output_data = Interpreter.output_tensor!(interpreter, 0)
    true = expected_out == output_data

    if print_state, do: TFLiteElixir.print_interpreter_state(interpreter)
    :ok
  end
end
