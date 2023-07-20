# Changelog

## main

### Changed
- [deps] Use libedgetpu v0.1.7.
- Use tensorflow v2.13.0.

## v0.3.2 (2022-04-03)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.2) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.2)

### Fixed
- [precompiled-nerves] Guess correct `TARGET_ARCH` from `TARGET_CPU`.

## v0.3.1 (2022-04-03)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.1) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.1)

### Fixed
- [deps] Use libedgetpu v0.1.6.

### Changed
- [examples] Examples moved to [cocoa-xu/tflite_elixir](https://github.com/cocoa-xu/tflite_elixir).

## v0.3.0 (2022-04-02)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.3.0) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.3.0)

### Breaking Change
- This repo will now be the TensorFlow Lite Erlang bindings. For Elixir bindings, please visit [cocoa-xu/tflite_elixir](https://github.com/cocoa-xu/tflite_elixir).

### Fixed
- [erlang] Generate correct error message from a list of errors.
- [c_src] Initialize resource pointers with `nullptr`.
- Implemented tokenizers for MobileBERT (#57) by @cocoa-xu.
- [make] Ensure priv dir exist.

## v0.2.1 (2022-04-02)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.2.1) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.2.1)

### Changed
- [deps] Use TensorFlow Lite version 2.11.1.

### Fixed
- [erlang] Generate correct error message from a list of errors.
- [c_src] Initialize resource pointers with `nullptr`.
- Implemented tokenizers for MobileBERT (#57) by @cocoa-xu.
- [make] Ensure priv dir exist.

## v0.2.0 (2022-03-30)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.2.0) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.2.0)

### Breaking Changes
- Renamed root namespace from `TFLiteElixir` to `TFLiteBEAM`

### Changes
- `buffer` will be copied and managed when using `TFLiteBEAM.FlatBufferModel.build_from_buffer/1`.
- `TFLiteBEAM.TFLiteTensor.dims/1` returns a list (following TensorFlow Lite's C++ API convention) while `TFLiteBEAM.TFLiteTensor.shape/1` returns a tuple (folllowing `nx`'s convention.)

### Added
- Erlang support.
- [example] added pose estimation example (#43) by @mnishiguchi
- [example] use thunder model instead of lightning in pose estimation (#45) by @mnishiguchi
- [example] added audio classification example
- Experimental high-level module `TFLiteBEAM.ImageClassification`.

  ```elixir
  iex> alias TFLiteBEAM.ImageClassification
  iex> {:ok, pid} = ImageClassification.start("test/test_data/mobilenet_v2_1.0_224_inat_bird_quant.tflite")
  iex> ImageClassification.predict(pid, "test/test_data/parrot.jpeg")
  %{class_id: 923, score: 0.70703125}
  iex> ImageClassification.set_label_from_associated_file(pid, "inat_bird_labels.txt")
  :ok
  iex> ImageClassification.predict(pid, "test/test_data/parrot.jpeg")
  %{class_id: 923, label: "Ara macao (Scarlet Macaw)", score: 0.70703125}
  iex> ImageClassification.predict(pid, "test/test_data/parrot.jpeg", top_k: 3)
  [
    %{class_id: 923, label: "Ara macao (Scarlet Macaw)", score: 0.70703125},
    %{
      class_id: 837,
      label: "Platycercus elegans (Crimson Rosella)",
      score: 0.078125
    },
    %{
      class_id: 245,
      label: "Coracias caudatus (Lilac-breasted Roller)",
      score: 0.01953125
    }
  ]
  ```

## v0.1.7 (2022-03-22)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.7) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.7)

### Breaking Changes
- Deprecated `TFLiteElixir.Interpreter.allocate_tensors!/1`
- Deprecated Access behaviour for `TFLiteElixir.FlatBufferModel`

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

### Changed
- Improve `TFLiteElixir.TFLiteTensor.to_nx/2` (#33) by @cocoa-xu
- [doc] Improve doc for to_nx (#31) by @mnishiguchi

### Added
- Implemented 
  - `FlatBufferModel.{list_associated_files/1,get_associated_file/2}`
  - `TFLiteElixir.Interpreter.signature_keys/1`
  - `TFLiteElixir.Interpreter.execution_plan/1`
  - `TFLiteElixir.Interpreter.new_from_buffer/1`
  - `TFLiteElixir.Interpreter.tensors_size/1`
  - `TFLiteElixir.Interpreter.variables/1`
  - `TFLiteElixir.Interpreter.set_variables/2`
  - `TFLiteElixir.Interpreter.set_inputs/2`
  - `TFLiteElixir.Interpreter.set_outputs/2`
- [example] object detection example (#40) by @mnishiguchi

## v0.1.6 (2022-03-19)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.6) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.6)

### Fixed
- [edgetpu] Improved edgetpu context handling, and bumped libedgetpu_runtime_version to v0.1.5. Fixed [#30](https://github.com/cocoa-xu/tflite_beam/issues/30)

### Added
- [example] artistic-style-transfer example (#27) @mnishiguchi

## v0.1.5 (2022-03-18)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.5) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.5)

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
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.4) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.4)

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
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.3) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.3)

### Changes
- Bump TFLite version to [v2.11.0](https://github.com/tensorflow/tensorflow/tree/v2.11.0).

## v0.1.2 (2022-03-08)
[Browse the Repository](https://github.com/cocoa-xu/tflite_beam/tree/v0.1.2) | [Released Assets](https://github.com/cocoa-xu/tflite_beam/releases/tag/v0.1.2)

First release on hex.pm.
