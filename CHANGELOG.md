# Changelog

## main

### Fixed
- Properly implemented `TFLiteElixir.FlatBufferModel.read_all_metadata/1`.

  ```elixir
  iex> filename = Path.join([__DIR__, "test", "test_data", "mobilenet_v2_1.0_224_inat_bird_quant.tflite"])
  iex> %FlatBufferModel{} = model = FlatBufferModel.build_from_buffer(File.read!(filename))
  iex> TFLiteElixir.FlatBufferModel.read_all_metadata(model)
  %{
    TFLITE_METADATA: %{
      description:
        "Identify the most prominent object in the image from a known set of categories.",
      min_parser_version: "1.0.0",
      name: "ImageClassifier",
      subgraph_metadata: [
        %{
          input_tensor_metadata: [
            %{
              content: %{
                content_properties: %{color_space: "RGB"},
                content_properties_type: "ImageProperties"
              },
              description: "Input image to be classified.",
              name: "image",
              process_units: [
                %{
                  options: %{mean: [127.5], std: [127.5]},
                  options_type: "NormalizationOptions"
                }
              ],
              stats: %{max: [255.0], min: [0.0]}
            }
          ],
          output_tensor_metadata: [
            %{
              associated_files: [
                %{
                  description: "Labels for categories that the model can recognize.",
                  name: "inat_bird_labels.txt",
                  type: "TENSOR_AXIS_LABELS"
                }
              ],
              description: "Probabilities of the labels respectively.",
              name: "probability",
              stats: %{max: [255.0], min: [0.0]}
            }
          ]
        }
      ]
    },
    min_runtime_version: "1.5.0"
  }
  ```

## v0.1.6 (2022-03-19)
[Browse the Repository](https://github.com/cocoa-xu/tflite_elixir/tree/v0.1.6) | [Released Assets](https://github.com/cocoa-xu/tflite_elixir/releases/tag/v0.1.6)

### Fixed
- [edgetpu] Improved edgetpu context handling, and bumped libedgetpu_runtime_version to v0.1.5. Fixed [#30](https://github.com/cocoa-xu/tflite_elixir/issues/30)

### Added
- [example] artistic-style-transfer example (#27) @mnishiguchi

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
