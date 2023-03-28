defmodule TFLiteBEAM.InterpreterBuilder.Test do
  use ExUnit.Case

  alias TFLiteBEAM.InterpreterBuilder
  alias TFLiteBEAM.Ops.Builtin.BuiltinResolver
  alias TFLiteBEAM.FlatBufferModel
  alias TFLiteBEAM.Interpreter

  test "get a new interpreter builder" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    assert is_reference(builder)
  end

  test "build interpreter" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()

    assert :ok == InterpreterBuilder.build!(builder, interpreter)
  end

  test "build interpreter with 2 threads" do
    filename = Path.join([__DIR__, "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
    model = FlatBufferModel.build_from_file(filename)
    resolver = BuiltinResolver.new!()
    builder = InterpreterBuilder.new!(model, resolver)
    interpreter = Interpreter.new!()

    assert :ok == InterpreterBuilder.set_num_threads!(builder, 2)
    assert :ok == InterpreterBuilder.build!(builder, interpreter)
  end
end
