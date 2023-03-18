# Changelog

## v0.1.5 (2022-03-18)
[Browse the Repository](https://github.com/cocoa-xu/tflite_elixir/tree/v0.1.5) | [Released Assets](https://github.com/cocoa-xu/tflite_elixir/releases/tag/v0.1.5)

### Breaking Changes
- Deprecated functions:
  - `TFLiteElixir.FlatBufferModel.initialized!/1`
  - `TFLiteElixir.FlatBufferModel.get_minimum_runtime!/1`
  - `TFLiteElixir.TFLiteTensor.tensor!`
  - `TFLiteElixir.TFLiteTensor.to_nx!`
  - `TFLiteElixir.TFLiteTensor.to_binary!`
  - `TFLiteElixir.FlatBufferModel.build_from_buffer!`
  - `TFLiteElixir.FlatBufferModel.get_full_signature_list`
- `TFLiteElixir.Coral.get_edge_tpu_context/1` now takes keyword options.

### Changes
- [example] Improve Inference on TPU notebook (#15) @mnishiguchi
- [example] Improve Inference on TPU notebook (#16) @mnishiguchi
- Alias modules in tflite_interpreter (#17) @mnishiguchi
- Rename elixir files based on module names (#18) @mnishiguchi
- add moduledocs (#19) @mnishiguchi

### Fixed
- Fixed a few places that could lead to segmentation fault.
- [example] Fixed broken ESRGAN link, Visualize the result section in the "Super Resolution" notebook. Lock down `tflite_elixir` and `evision` version (#29) @mnishiguchi.
- [typespec] Fixed typespec for `TFLiteElixir.Coral.edge_tpu_devices/0` (#22) @mnishiguchi.

### Added
- [test] Unit tests for `TFLiteElixir.Interpreter`, `TFLiteElixir.InterpreterBuilder` and `TFLiteElixir.Ops.Builtin.BuiltinResolver`.
- [example] Added intro text to super_resolution_example. (#26) @mnishiguchi.
- `TFLiteElixir.FlatBufferModel.error_reporter/1`.
- `TFLiteElixir.FlatBufferModel.verify_and_build_from_file/2`

### Misc
- Simple workaround for cortex-a53 and cortex-a57, `vcvtaq_s32_f32`.

## v0.1.4 (2022-03-14)
[Browse the Repository](https://github.com/cocoa-xu/tflite_elixir/tree/v0.1.4) | [Released Assets](https://github.com/cocoa-xu/tflite_elixir/releases/tag/v0.1.4)

### Breaking Changes
- Snake case functions (#21) @mnishiguchi

### Changes
- [example] Improve Inference on TPU notebook (#15) @mnishiguchi
- [example] Improve Inference on TPU notebook (#16) @mnishiguchi
- Alias modules in tflite_interpreter (#17) @mnishiguchi
- Rename elixir files based on module names (#18) @mnishiguchi
- add moduledocs (#19) @mnishiguchi

### Fixed
- Fix compilation logic when not using precompiled binaries.

### Added
- Implemented `TFLiteElixir.reset_variable_tensor/1`.
- Add support for armv6.

### Misc
- Simple workaround for cortex-a53 and cortex-a57, `vcvtaq_s32_f32`.

## v0.1.3 (2022-03-09)
[Browse the Repository](https://github.com/cocoa-xu/tflite_elixir/tree/v0.1.3) | [Released Assets](https://github.com/cocoa-xu/tflite_elixir/releases/tag/v0.1.3)

### Changes
- Bump TFLite version to [v2.11.0](https://github.com/tensorflow/tensorflow/tree/v2.11.0).

## v0.1.2 (2022-03-08)
[Browse the Repository](https://github.com/cocoa-xu/tflite_elixir/tree/v0.1.2) | [Released Assets](https://github.com/cocoa-xu/tflite_elixir/releases/tag/v0.1.2)

First release on hex.pm.
