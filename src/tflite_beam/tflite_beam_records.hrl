-record(tflite_beam_error_reporter, {ref :: reference()}).
-record(tflite_beam_flatbuffer_model, {initialized :: boolean(), minimum_runtime :: binary(), ref :: reference()}).
-record(tflite_beam_quantization_params, {scale, zero_point, quantized_dimension}).

-type tflite_beam_tensor_type() :: no_type
                                | {f, 32}
                                | {s, 32}
                                | {u, 8}
                                | {s, 64}
                                | string
                                | bool
                                | {s, 16}
                                | {c, 64}
                                | {s, 8}
                                | {f, 16}
                                | {f, 64}
                                | {c, 128}
                                | {u, 64}
                                | resource
                                | variant
                                | {u, 32}.
-record(tflite_beam_tensor, {name :: binary(), index :: non_neg_integer(), shape :: tuple(), shape_signature :: list(), type :: tflite_beam_tensor_type(), quantization_params :: #tflite_beam_quantization_params{}, sparsity_params, ref :: reference()}).
