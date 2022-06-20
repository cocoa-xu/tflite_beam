# TFLite-Elixir [WIP]

There are mainly two reasons why I write this library.

1. `Port` has limited throughput.
2. It would be easier and more flexible to make changes in Elixir if we have all the basic building blocks (i.e., functions)
from TensorFlow Lite available in Elixir.

[![Coverage Status](https://coveralls.io/repos/github/cocoa-xu/tflite_elixir/badge.svg?branch=main)](https://coveralls.io/github/cocoa-xu/tflite_elixir?branch=main)

| OS               | arch    | Build Status |
|------------------|---------|--------------|
| Ubuntu 20.04     | x86_64  | [![CI](https://github.com/cocoa-xu/tflite_elixir/actions/workflows/linux-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/tflite_elixir/actions/workflows/linux-x86_64.yml) |
| macOS 11 Big Sur | x86_64  | [![CI](https://github.com/cocoa-xu/tflite_elixir/actions/workflows/macos-x86_64.yml/badge.svg)](https://github.com/cocoa-xu/tflite_elixir/actions/workflows/macos-x86_64.yml) |

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
- Or use pre-converted input [parrot.bin](https://github.com/cocoa-xu/tflite_elixir/blob/main/test/test_data/parrot.bin)

Labels: [inat_bird_labels.txt](https://github.com/google-coral/edgetpu/blob/master/test_data/inat_bird_labels.txt)

```elixir
alias Evision, as: Cv
alias TFLiteElixir, as: TFLite

# load labels
labels = File.read!("inat_bird_labels.txt") |> String.split("\n")

# load tflite model
filename = "mobilenet_v2_1.0_224_inat_bird_quant.tflite"
model = TFLite.FlatBufferModel.buildFromFile!(filename)
resolver = TFLite.Ops.Builtin.BuiltinResolver.new!()
builder = TFLite.InterpreterBuilder.new!(model, resolver)
interpreter = TFLite.Interpreter.new!()
:ok = TFLite.InterpreterBuilder.build!(builder, interpreter)
:ok = TFLite.Interpreter.allocateTensors!(interpreter)

# verify loaded model, feel free to skip
# [0] = TFLite.Interpreter.inputs!(interpreter)
# [171] = TFLite.Interpreter.outputs!(interpreter)
# "map/TensorArrayStack/TensorArrayGatherV3" = TFLite.Interpreter.getInputName!(interpreter, 0)
# "prediction" = TFLite.Interpreter.getOutputName!(interpreter, 0)
# input_tensor = TFLite.Interpreter.tensor!(interpreter, 0)
# [1, 224, 224, 3] = TFLite.TfLiteTensor.dims!(input_tensor)
# {:u, 8} = TFLite.TfLiteTensor.type(input_tensor)
# output_tensor = TFLite.Interpreter.tensor!(interpreter, 171)
# [1, 965] = TFLite.TfLiteTensor.dims!(output_tensor)
# {:u, 8} = TFLite.TfLiteTensor.type(output_tensor)

# parrot.bin - if you don't have :evision
binary = File.read!("parrot.bin")
# parrot.jpg - if you have :evision
# load image, resize it, covert to RGB and to binary 
binary = 
  Cv.imread!("parrot.jpg")
  |> Cv.resize([224, 224])
  |> Cv.cvtColor!(Cv.cv_COLOR_BGR2RGB)
  |> Cv.Mat.to_binary(mat)

# set input, run forwarding, get output
TFLite.Interpreter.input_tensor(interpreter, 0, binary)
TFLite.Interpreter.invoke(interpreter)
output_data = TFLite.Interpreter.output_tensor!(interpreter, 0)

# if you have :nx
# get predicted label
output_data
|> Nx.from_binary({:u, 8})
|> Nx.argmax()
|> Nx.to_scalar()
|> then(&Enum.at(labels, &1))
```

## Coral Support
### Dependencies
For macOS
```shell
# for compiling libusb
brew install autoconf automake
```

For DEBIAN/Ubuntu
```shell
# to change rpath
sudo apt install patchelf
```

For some Linux OSes you need to manually execute the following command to update udev rules, otherwise, libedgetpu will
fail to initialize Coral devices.

```shell
# possible values for these env vars are listed in the next section
export TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME_RELEASE_NAME=grouper
export TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME=edgetpu_runtime_20220308

mix deps.get
bash "3rd_party/cache/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME}/edgetpu_runtime/install.sh"
```

### Compile-Time Environment Variable
- `TFLITE_ELIXIR_CORAL_SUPPORT`

  Enable Coral Support.

  Default to `YES`.

- `TFLITE_ELIXIR_CORAL_USB_THROTTLE`

  Throttling USB Coral Devices. Please see the official warning here, [google-coral/libedgetpu](https://github.com/google-coral/libedgetpu#warning).
  
  Default value is `YES`.
  
  Note that only when `TFLITE_ELIXIR_CORAL_USB_THROTTLE` is set to `NO`, `:tflite_elixir` will use the non-throttled libedgetpu libraries.

- `TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME`

  Select the [libedgetpu runtime](https://coral.ai/software/#edgetpu-runtime).

  Default runtime version is `edgetpu_runtime_20220308`.

 `TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME_RELEASE_NAME`

  Check the [google-coral/libedgetpu](https://github.com/google-coral/libedgetpu) for more details.

  Default release name is `grouper`.

  | Release Name | Runtime Version            | 
  |--------------|----------------------------|
  | `grouper`    | `edgetpu_runtime_20220308` |
  | `frogfish`   | `edgetpu_runtime_20210119` |
  | `frogfish`   | `edgetpu_runtime_20201204` |
  | `frogfish`   | `edgetpu_runtime_20201105` |
  | `frogfish`   | `edgetpu_runtime_20200728` |
  | `frogfish`   | `edgetpu_runtime_20200331` |
  | `frogfish`   | `edgetpu_runtime_20200128` |

  For macOS, `TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME_RELEASE_NAME=grouper` and `TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME=edgetpu_runtime_20220308`

  For Linux, `edgetpu_runtime_20220308` seems to have some compatibility issues. `TFLiteElixir.Coral.makeEdgeTpuContext/2` is likely to fail.

  Using `edgetpu_runtime_20210119` solves the issue for now on Linux. 

  ```shell
  export TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME_RELEASE_NAME=frogfish
  export TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME=edgetpu_runtime_20210119
  ```

- `TFLITE_ELIXIR_CORAL_LIBEDGETPU_LIBRARIES`
  
  Choose which ones of the libedgetpu libraries to copy to the `priv` directory of the `:elixir_coral` app.

  Default value is `native` - only native libraries will be copied. `native` corresponds to the host OS and CPU architecture when compiling this library.

  When set to a specific value, e.g, `darwin_arm64` or `darwin_x86_64`, then the corresponding one will be copied. This option is expected to be used for cross-compiling. 
  Available values for this option are:

  | Value            | OS/CPU              |
  |------------------|---------------------|
  | `aarch64`        | Linux arm64         |
  | `armv7a`         | Linux armv7         |
  | `k8`             | Linux x86_64        |
  | `darwin_arm64`   | macOS Apple Silicon |
  | `darwin_x86_64`  | macOS x86_64        |
  | `x64_windows`    | Windows x86_64      |


- `TFLITE_ELIXIR_CACHE_DIR`
  
  Cache directory for the runtime zip file.

  Default value is `./3rd_party/cache`.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `tflite_elixir` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:tflite_elixir, "~> 0.1.0", github: "cocoa-xu/tflite_elixir"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/tflite_elixir>.

