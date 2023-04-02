# TFLiteBEAM

TensorFlow Lite BEAM bindings with optional EdgeTPU support.

[![Coverage Status](https://coveralls.io/repos/github/cocoa-xu/tflite_beam/badge.svg?branch=main)](https://coveralls.io/github/cocoa-xu/tflite_beam?branch=main)

| OS               | Arch    | ABI       | Build Status | Has Precompiled Library |
|------------------|---------|-----------|--------------|-------------------------|
| Ubuntu 20.04     | x86_64  | gnu       | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-x86_64.yml) | Yes |
| Ubuntu 20.04     | arm64   | gnu       | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml) | Yes |
| Ubuntu 20.04     | armv7l  | gnueabihf | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml) | Yes |
| Ubuntu 20.04     | riscv64 | gnu       | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml) | Yes |
| macOS 11 Big Sur | x86_64  | darwin    | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/macos-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/macos-x86_64.yml) | Yes |
| macOS 11 Big Sur | arm64   | darwin    | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/macos-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/macos-precompile.yml) | Yes |

## Try it in Livebook
A general workflow looks like this,

```elixir
# will download and install precompiled version
Mix.install([
  {:tflite_beam, "~> 0.2.1"}
])

# parrot.jpeg and the tflite file can be found in the test/test_data directory
interpreter = TFLiteBEAM.Interpreter.new!("/path/to/mobilenet_v2_1.0_224_inat_bird_quant.tflite")
input =
  StbImage.read_file!("/path/to/parrot.jpeg")
  |> StbImage.resize(224, 224)
  |> StbImage.to_nx()

[output_tensor_0] = TFLiteBEAM.Interpreter.predict(interpreter, input)
nx_tensor =
  TFLiteBEAM.TFLiteTensor.to_binary(output_tensor_0)
  |> Nx.from_binary(:u8)

# get top k predictions (numerical id of the class)
# classes can be found in this file,
# https://raw.githubusercontent.com/cocoa-xu/tflite_beam/main/test/test_data/inat_bird_labels.txt
# each line corresponds to a class
# and the first line = id 0
top_k = 5
sorted_indices = Nx.argsort(nx_tensor, direction: :desc)
top_k_indices = Nx.take(sorted_indices, Nx.iota({top_k}))
top_k_preds = Nx.to_flat_list(top_k_indices)
```

And there is an experimental `ImageClassification` module that does everything for you. It supports both CPU and TPU, and it will show more information, including scores (confidence) and the class name of the predicted results. It's also more flexible where you can adjust different parameters like `top_k` and `threshold` (for confidence) and etc.

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

## Nerves Support

### Prebuilt firmware (Experimental)

[![Nerves](https://github-actions.40ants.com/cocoa-xu/tflite_beam/matrix.svg?only=nerves-build)](https://github.com/cocoa-xu/tflite_beam)

Prebuilt firmwares are available [here](https://github.com/cocoa-xu/tflite_beam/actions/workflows/nerves-build.yml?query=is%3Asuccess).

Select the most recent run and scroll down to the `Artifacts` section, download the firmware file for your board and run

```bash
fwup /path/to/the/downloaded/firmware.fw
```

In the nerves build, `tflite_beam` is integrated as one of the dependencies of the [nerves_livebook](https://github.com/livebook-dev/nerves_livebook) project. This means that you can use livebook (as well as other pre-pulled libraries) to explore and evaluate the `tflite_beam` project.

The default password of the livebook is `nerves` (as the time of writing, if it does not work, please check the nerves_livebook project).

### Build from Source

1. If prefer precompiled binaries
```shell
# for example
export MIX_TARGET=rpi4

# There is no need to explicitly set CPU architecture
#   for the precompiled libedgetpu binaries. The arch
#   is automatically detected by the `TARGET_ARCH`,
#   `TARGET_OS` and `TARGET_ABI` environment vars.
#
# However, if you are using your own nerves target
#   you can manually set the correct arch, e.g.,
#   set `aarch64` for rpi4.
#
# Possible values including
# - aarch64
# - armv7l
# - riscv64
# - x86_64
export TFLITE_ELIXIR_CORAL_LIBEDGETPU_LIBRARIES=aarch64
```

2. If prefer not to use precompiled binaries
```shell
# for example
export MIX_TARGET=rpi4
# then set env var TFLITE_ELIXIR_PREFER_PRECOMPILED to NO
export TFLITE_ELIXIR_PREFER_PRECOMPILED=NO
```

## Demo
### Mix Task Demo
0. List all available Edge TPU
```shell
mix list_edgetpu
```

1. Image classification
```shell
mix help classify_image

# Note: The first inference on Edge TPU is slow because it includes,
# loading the model into Edge TPU memory
mix classify_image \
  --model test/test_data/mobilenet_v2_1.0_224_inat_bird_quant.tflite \
  --input test/test_data/parrot.jpeg \
  --labels test/test_data/inat_bird_labels.txt
```

Output from the mix task
```
----INFERENCE TIME----
Note: The first inference on Edge TPU is slow because it includes, loading the model into Edge TPU memory.
6.7ms
-------RESULTS--------
Ara macao (Scarlet Macaw): 0.70703
```

2. Object detection
```shell
mix help detect_image

# Note: The first inference on Edge TPU is slow because it includes,
# loading the model into Edge TPU memory
mix detect_image \
  --model test/test_data/ssd_mobilenet_v2_coco_quant_postprocess.tflite \
  --input test/test_data/cat.jpeg \
  --labels test/test_data/coco_labels.txt
```

Output from the mix task
```
INFO: Created TensorFlow Lite XNNPACK delegate for CPU.
----INFERENCE TIME----
13.2ms
cat
  id   : 16
  score: 0.953
  bbox : [3, -1, 294, 240]
```

test files used here are downloaded from [google-coral/test_data](https://github.com/google-coral/test_data) and [wikipedia](https://commons.wikimedia.org/wiki/File:Cat03.jpg).

### Demo code
Model: [mobilenet_v2_1.0_224_inat_bird_quant.tflite](https://github.com/google-coral/edgetpu/blob/master/test_data/mobilenet_v2_1.0_224_inat_bird_quant.tflite)

Input image:
- [parrot.jpg](https://github.com/google-coral/edgetpu/blob/master/test_data/parrot.jpg)
- Or use pre-converted input [parrot.bin](https://github.com/cocoa-xu/tflite_beam/blob/main/test/test_data/parrot.bin)

Labels: [inat_bird_labels.txt](https://github.com/google-coral/edgetpu/blob/master/test_data/inat_bird_labels.txt)

```elixir
alias Evision, as: Cv
alias TFLiteBEAM, as: TFLite

# load labels
labels = File.read!("inat_bird_labels.txt") |> String.split("\n")

# load tflite model
filename = "mobilenet_v2_1.0_224_inat_bird_quant.tflite"
model = TFLite.FlatBufferModel.build_from_file(filename)
resolver = TFLite.Ops.Builtin.BuiltinResolver.new!()
builder = TFLite.InterpreterBuilder.new!(model, resolver)
interpreter = TFLite.Interpreter.new!()
:ok = TFLite.InterpreterBuilder.build!(builder, interpreter)
:ok = TFLite.Interpreter.allocate_tensors(interpreter)

# verify loaded model, feel free to skip
# [0] = TFLite.Interpreter.inputs!(interpreter)
# [171] = TFLite.Interpreter.outputs!(interpreter)
# "map/TensorArrayStack/TensorArrayGatherV3" = TFLite.Interpreter.get_input_name!(interpreter, 0)
# "prediction" = TFLite.Interpreter.get_output_name!(interpreter, 0)
# input_tensor = TFLite.Interpreter.tensor(interpreter, 0)
# [1, 224, 224, 3] = TFLite.TFLiteTensor.dims(input_tensor)
# {:u, 8} = TFLite.TFLiteTensor.type(input_tensor)
# output_tensor = TFLite.Interpreter.tensor(interpreter, 171)
# [1, 965] = TFLite.TFLiteTensor.dims(output_tensor)
# {:u, 8} = TFLite.TFLiteTensor.type(output_tensor)

# parrot.bin - if you don't have :evision
binary = File.read!("parrot.bin")
# parrot.jpg - if you have :evision
# load image, resize it, covert to RGB and to binary
binary =
  Cv.imread("parrot.jpg")
  |> Cv.resize({224, 224})
  |> Cv.cvtColor(Cv.cv_COLOR_BGR2RGB)
  |> Cv.Mat.to_binary(mat)

# set input, run forwarding, get output
TFLite.Interpreter.input_tensor(interpreter, 0, binary)
TFLite.Interpreter.invoke(interpreter)
output_data = TFLite.Interpreter.output_tensor!(interpreter, 0)

# if you have :nx
# get predicted label
output_data
|> Nx.from_binary(:u8)
|> Nx.argmax()
|> Nx.to_scalar()
|> then(&Enum.at(labels, &1))
```

## Coral Support
### Dependencies
For macOS
```shell
# only required if not using precompiled binaries
# for compiling libusb
brew install autoconf automake
```

For some Linux OSes you need to manually execute the following command to update udev rules, otherwise, libedgetpu will fail to initialize Coral devices.

```shell
mix deps.get
bash "3rd_party/cache/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME}/edgetpu_runtime/install.sh"
```

### Compile-Time Environment Variable
- `TFLITE_ELIXIR_PREFER_PRECOMPILED`

  Use precompiled binaries when `TFLITE_ELIXIR_PREFER_PRECOMPILED` is `YES`. Otherwise, this library will compile from source.

  Defaults to `YES`.

- `TFLITE_ELIXIR_CORAL_SUPPORT`

  Enable Coral Support.

  Defaults to `YES`.

- `TFLITE_ELIXIR_CORAL_USB_THROTTLE`

  Throttling USB Coral Devices. Please see the official warning here, [google-coral/libedgetpu](https://github.com/google-coral/libedgetpu#warning).

  Defaults to `YES`.

  Note that only when `TFLITE_ELIXIR_CORAL_USB_THROTTLE` is set to `NO`, `:tflite_beam` will use the non-throttled libedgetpu libraries.

- `TFLITE_ELIXIR_CORAL_LIBEDGETPU_LIBRARIES`

  Choose which ones of the libedgetpu libraries to copy to the `priv` directory of the `:tflite_beam` app.

  Default value is `native` - only native libraries will be downloaded and copied. `native` corresponds to the host OS and CPU architecture when compiling this library.

  When set to a specific value, e.g, `darwin_arm64` or `darwin_x86_64`, then the corresponding one will be downloaded and copied. This option is expected to be used for cross-compiling, like with nerves.

  Available values for this option are:

  | Value            | OS/CPU              |
  |------------------|---------------------|
  | `aarch64`        | Linux arm64         |
  | `armv7l`         | Linux armv7         |
  | `armv6`          | Linux armv6         |
  | `k8`             | Linux x86_64        |
  | `x86_64`         | Linux x86_64        |
  | `riscv64`        | Linux riscv64       |
  | `darwin_arm64`   | macOS Apple Silicon |
  | `darwin_x86_64`  | macOS x86_64        |


## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `tflite_beam` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:tflite_beam, "~> 0.1"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/tflite_beam>.

