defmodule TFLiteElixir.TFLiteQuantizationParams do
  defstruct [:scale, :zero_point, :quantized_dimension]
end
