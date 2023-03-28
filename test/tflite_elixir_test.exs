defmodule TFLiteBEAM.Test do
  use ExUnit.Case

  alias TFLiteBEAM.Interpreter
  alias TFLiteBEAM.TFLiteTensor

  test "print_interpreter_state/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = TFLiteBEAM.Interpreter.new!(filename)

    assert nil == TFLiteBEAM.print_interpreter_state(interpreter)
  end

  test "reset_variable_tensor/1" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    interpreter = Interpreter.new!(filename)
    t = Interpreter.tensor(interpreter, 0)

    zeros = Nx.broadcast(Nx.tensor(0, type: :u8), {1, 224, 224, 3})
    ones = Nx.broadcast(Nx.tensor(1, type: :u8), {1, 224, 224, 3})

    TFLiteTensor.set_data(t, ones)
    t = Interpreter.tensor(interpreter, 0)
    assert Nx.all_close(ones, TFLiteTensor.to_nx(t, backend: Nx.BinaryBackend))

    TFLiteBEAM.reset_variable_tensor(t)
    t = Interpreter.tensor(interpreter, 0)
    assert Nx.all_close(zeros, TFLiteTensor.to_nx(t, backend: Nx.BinaryBackend))
  end

  with {:module, TFLiteBEAM.Coral} <- Code.ensure_compiled(TFLiteBEAM.Coral) do
    test "Contains EdgeTpu Custom Op" do
      filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
      model = TFLiteBEAM.FlatBufferModel.build_from_buffer(File.read!(filename))
      ret = TFLiteBEAM.Coral.contains_edge_tpu_custom_op?(model)

      :ok =
        case ret do
          false ->
            :ok

          {:error,
           "Coral support is disabled when compiling this library. Please enable Coral support and recompile this library."} ->
            :ok

          _other ->
            false
        end

      filename =
        Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant_edgetpu.tflite"])

      model = TFLiteBEAM.FlatBufferModel.build_from_buffer(File.read!(filename))
      ret = TFLiteBEAM.Coral.contains_edge_tpu_custom_op?(model)

      :ok =
        case ret do
          true ->
            :ok

          {:error,
           "Coral support is disabled when compiling this library. Please enable Coral support and recompile this library."} ->
            :ok

          _other ->
            false
        end
    end
  end
end
