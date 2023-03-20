defmodule TFLiteElixir.TFLiteTensor.Test do
  use ExUnit.Case

  alias TFLiteElixir.Interpreter
  alias TFLiteElixir.TFLiteTensor

  test "type/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)

    t = Interpreter.tensor(interpreter, 0)
    assert {:u, 8} == TFLiteTensor.type(t)
    assert {:u, 8} == TFLiteTensor.type(t.reference)
  end

  test "type/1 with invalid reference" do
    assert {:error, "cannot access NifResTfLiteTensor resource"} == TFLiteTensor.type(make_ref())
  end

  test "dims/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)

    t = Interpreter.tensor(interpreter, 0)
    assert [1, 224, 224, 3] == TFLiteTensor.dims(t)
    assert [1, 224, 224, 3] == TFLiteTensor.dims(t.reference)
  end

  test "dims/1 with invalid reference" do
    assert {:error, "cannot access NifResTfLiteTensor resource"} == TFLiteTensor.dims(make_ref())
  end

  test "quantization_params/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)
    t = Interpreter.tensor(interpreter, 0)

    assert %TFLiteElixir.TFLiteQuantizationParams{
             scale: [0.0078125],
             zero_point: [128],
             quantized_dimension: 0
           } == TFLiteTensor.quantization_params(t)

    assert %TFLiteElixir.TFLiteQuantizationParams{
             scale: [0.0078125],
             zero_point: [128],
             quantized_dimension: 0
           } == TFLiteTensor.quantization_params(t.reference)
  end

  test "quantization_params/1 with invalid reference" do
    assert {:error, "cannot access NifResTfLiteTensor resource"} ==
             TFLiteTensor.quantization_params(make_ref())
  end

  test "set_data/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)
    t = Interpreter.tensor(interpreter, 0)

    zeros = Nx.broadcast(Nx.tensor(0, type: :u8), {1, 224, 224, 3})
    zeros_binary = Nx.to_binary(zeros)
    ones = Nx.broadcast(Nx.tensor(1, type: :u8), {1, 224, 224, 3})
    ones_binary = Nx.to_binary(ones)

    TFLiteTensor.set_data(t, ones)
    t = Interpreter.tensor(interpreter, 0)
    assert Nx.all_close(ones, TFLiteTensor.to_nx(t, backend: Nx.BinaryBackend))

    TFLiteTensor.set_data(t, zeros_binary)
    t = Interpreter.tensor(interpreter, 0)
    assert Nx.all_close(zeros, TFLiteTensor.to_nx(t, backend: Nx.BinaryBackend))

    TFLiteTensor.set_data(t.reference, ones_binary)
    t = Interpreter.tensor(interpreter, 0)
    assert Nx.all_close(ones, TFLiteTensor.to_nx(t, backend: Nx.BinaryBackend))

    TFLiteTensor.set_data(t.reference, zeros)
    t = Interpreter.tensor(interpreter, 0)
    assert Nx.all_close(zeros, TFLiteTensor.to_nx(t, backend: Nx.BinaryBackend))
  end

  test "to_binary/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)
    t = Interpreter.tensor(interpreter, 0)

    ones = Nx.broadcast(Nx.tensor(1, type: :u8), {1, 224, 224, 3})
    ones_binary = Nx.to_binary(ones)

    TFLiteTensor.set_data(t, ones)
    t = Interpreter.tensor(interpreter, 0)
    assert ones_binary == TFLiteTensor.to_binary(t)
  end

  test "to_binary/2 with invalid reference" do
    assert {:error, "cannot access NifResTfLiteTensor resource"} ==
             TFLiteTensor.to_binary(make_ref())
  end

  test "to_nx/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)
    :ok = Interpreter.allocate_tensors(interpreter)

    t = Interpreter.tensor(interpreter, 0)
    %Nx.Tensor{} = nx_tensor = TFLiteTensor.to_nx(t)
    assert nx_tensor.shape == {1, 224, 224, 3}
    assert Nx.all_close(nx_tensor, 0)
  end

  test "to_nx/2" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)
    t = Interpreter.tensor(interpreter, 0)

    %Nx.Tensor{} = nx_tensor = TFLiteTensor.to_nx(t, backend: Nx.BinaryBackend)
    assert nx_tensor.shape == {1, 224, 224, 3}
    assert Nx.all_close(nx_tensor, 0)

    t = Interpreter.tensor(interpreter, 0)
    %Nx.Tensor{} = nx_tensor = TFLiteTensor.to_nx(t.reference, backend: Nx.BinaryBackend)
    assert nx_tensor.shape == {1, 224, 224, 3}
    assert Nx.all_close(nx_tensor, 0)
  end

  test "to_nx/2 with backend: nil" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)
    t = Interpreter.tensor(interpreter, 0)

    %Nx.Tensor{} = nx_tensor = TFLiteTensor.to_nx(t, backend: nil)
    assert nx_tensor.shape == {1, 224, 224, 3}
    assert Nx.all_close(nx_tensor, 0)
  end

  test "to_nx/2 with wrong backend" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)
    t = Interpreter.tensor(interpreter, 0)

    assert_raise RuntimeError,
                 "Expecting keyword parameter `backend` to be a module, however, got `42`",
                 fn ->
                   TFLiteTensor.to_nx(t, backend: 42)
                 end

    t = Interpreter.tensor(interpreter, 0)

    assert_raise RuntimeError,
                 "Expecting keyword parameter `backend` to be a module, however, got `true`",
                 fn ->
                   TFLiteTensor.to_nx(t, backend: true)
                 end
  end
end
