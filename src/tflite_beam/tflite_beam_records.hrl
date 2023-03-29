-record(tflite_beam_error_reporter, {ref}).
-record(tflite_beam_flatbuffer_model, {initialized, minimum_runtime, ref}).
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
