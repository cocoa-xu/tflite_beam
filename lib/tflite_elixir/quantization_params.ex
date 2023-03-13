defmodule TFLiteElixir.TFLiteQuantizationParams do
  @moduledoc """
  Quantization parameters that corresponds to the table QuantizationParameters
  in the [TFLite Model schema file].

  [TFLite Model schema file]: https://github.com/tensorflow/tensorflow/blob/master/tensorflow/lite/schema/schema.fbs
  """
  defstruct [:scale, :zero_point, :quantized_dimension]
end
