defmodule TFLiteElixir.Test do
  use ExUnit.Case

  test "TFLite.Interpreter.new(model_path)" do
    model_path = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    _interpreter = TFLiteElixir.Interpreter.new!(model_path)

    {error_at_stage, {:error, reason}} = TFLiteElixir.Interpreter.new("/dev/null")
    assert :build_from_file == error_at_stage
    assert reason == "cannot load flat buffer model from file"
  end

  with {:module, TFLiteElixir.Coral} <- Code.ensure_compiled(TFLiteElixir.Coral) do
    test "Contains EdgeTpu Custom Op" do
      filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
      model = TFLiteElixir.FlatBufferModel.build_from_buffer!(File.read!(filename))
      ret = TFLiteElixir.Coral.contains_edge_tpu_custom_op?(model)

      :ok =
        case ret do
          false ->
            :ok

          {:error,
           "Coral support is disabled when compiling this library. Please enable Coral support and recompile this library."} ->
            :ok

          other ->
            false
        end

      filename =
        Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant_edgetpu.tflite"])

      model = TFLiteElixir.FlatBufferModel.build_from_buffer!(File.read!(filename))
      ret = TFLiteElixir.Coral.contains_edge_tpu_custom_op?(model)

      :ok =
        case ret do
          true ->
            :ok

          {:error,
           "Coral support is disabled when compiling this library. Please enable Coral support and recompile this library."} ->
            :ok

          other ->
            false
        end
    end
  end
end
