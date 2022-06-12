defmodule TFLiteElixir.Coral do
  alias TFLiteElixir.FlatBufferModel, as: FlatBufferModel
  def containsEdgeTpuCustomOp?(%FlatBufferModel{model: model}) do
    TFLiteElixir.Nif.coral_contains_edgetpu_custom_op(model)
  end
end
