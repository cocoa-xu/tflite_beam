# Changelog

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
