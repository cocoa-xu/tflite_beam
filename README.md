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
bash "3rd_party/cache/${TFLITE_BEAM_CORAL_LIBEDGETPU_RUNTIME}/edgetpu_runtime/install.sh"
```

### Compile-Time Environment Variable
- `TFLITE_BEAM_PREFER_PRECOMPILED`

  Use precompiled binaries when `TFLITE_BEAM_PREFER_PRECOMPILED` is `true`. Otherwise, this library will compile from source.

  Defaults to `true`.

- `TFLITE_BEAM_CORAL_SUPPORT`

  Enable Coral Support.

  Defaults to `true`.

- `TFLITE_BEAM_CORAL_USB_THROTTLE`

  Throttling USB Coral Devices. Please see the official warning here, [google-coral/libedgetpu](https://github.com/google-coral/libedgetpu#warning).

  Defaults to `true`.

  Note that only when `TFLITE_BEAM_CORAL_USB_THROTTLE` is set to `NO`, `:tflite_beam` will use the non-throttled libedgetpu libraries.

- `TFLITE_BEAM_CORAL_LIBEDGETPU_LIBRARIES`

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
by adding `tflite_beam` to your list of dependencies in `rebar.config`:

```erlang
{deps, [
  {tflite_beam, "0.3.2"}
]}
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at <https://hexdocs.pm/tflite_beam>.

