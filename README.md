# TFLiteBEAM

TensorFlow Lite BEAM bindings with optional EdgeTPU support.

[![Hex.pm](https://img.shields.io/hexpm/v/tflite_beam.svg?style=flat&color=blue)](https://hex.pm/packages/tflite_beam)
[![Coverage Status](https://coveralls.io/repos/github/cocoa-xu/tflite_beam/badge.svg?branch=main)](https://coveralls.io/github/cocoa-xu/tflite_beam?branch=main)

| OS               | Arch    | ABI       | Build Status | Has Precompiled Library |
|------------------|---------|-----------|--------------|-------------------------|
| Ubuntu 20.04     | x86_64  | gnu       | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml) | Yes |
| Ubuntu 20.04     | arm64   | gnu       | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml) | Yes |
| Ubuntu 20.04     | armv7l  | gnueabihf | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml) | Yes |
| Ubuntu 20.04     | armv6   | gnueabihf | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml) | Yes |
| Ubuntu 20.04     | riscv64 | gnu       | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/linux-precompile.yml) | Yes |
| macOS 15 Sequoia | x86_64  | darwin    | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/macos-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/macos-precompile.yml) | Yes |
| macOS 14 Sonoma  | arm64   | darwin    | [![CI](https://github.com/cocoa-xu/tflite_beam/actions/workflows/macos-precompile.yml/badge.svg)](https://github.com/cocoa-xu/tflite_beam/actions/workflows/macos-precompile.yml) | Yes |


## Delegates

`tflite_beam_interpreter_builder:build/2` attaches an XNNPACK delegate for you,
unless you have attached one yourself. TfLite would otherwise apply XNNPACK on its
own, invisibly, inside `allocate_tensors/1` -- with a thread count nothing could
reach and no way to decline it. The acceleration is the same; where it happens is
now visible, and `set_num_threads/2` still reaches it.

```erlang
{ok, Resolver} = tflite_beam_ops_builtin_builtin_resolver:new(),
{ok, Builder} = tflite_beam_interpreter_builder:new(Model, Resolver),

%% your own delegate instead of the default one
{ok, Delegate} = tflite_beam_delegate:xnnpack(#{num_threads => 4}),
ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),

ok = tflite_beam_interpreter_builder:build(Builder, Interpreter).
```

`tflite_beam_delegate:available/0` lists the delegate kinds this build can create:
XNNPACK on every target except armv6 and armv7l, where nothing is attached and
inference runs as it always has.

To go back to TfLite delegating by itself, ask the resolver for it:

```erlang
{ok, Resolver} = tflite_beam_ops_builtin_builtin_resolver:new(#{apply_default_delegates => true}),
```

A delegate must outlive every interpreter built from the builder it was added to,
so there is no way to detach or free one: the builder and each interpreter hold it
for as long as they need it, and it goes when they do.

### Delegates from a shared library

Anything implementing TfLite's delegate plugin interface --
`tflite_plugin_create_delegate` and `tflite_plugin_destroy_delegate` -- can be
loaded at runtime, which covers Edge TPU, a GPU delegate built elsewhere, and
vendor delegates this library knows nothing about:

```erlang
{ok, Delegate} = tflite_beam_delegate:external("/opt/lib/libvendor_delegate.so",
                                               #{device => 0, precision => fp16}),
ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate).
```

Options are handed to the plugin as strings, which is the whole of that ABI, so
atoms and integers are converted and at most 256 pairs fit. What the keys mean is
the plugin's business. The path is resolved to an absolute one before loading,
because the loader is asked for exactly the file named -- a bare `libfoo.so` would
otherwise be searched for wherever the system looks, which is rarely where anyone
means. The library is not unloaded afterwards.

## Threading

An interpreter, and any delegate attached to it, belongs to one process at a time.
TfLite documents `tflite::Interpreter` as not thread-safe and leaves serialising access
to the caller, and nothing here adds a lock of its own -- `invoke/1` runs on a dirty
scheduler, so two processes sharing one interpreter really do run it on two OS threads
at once. Delegates are the same: nothing documents a `TfLiteDelegate` as safe to back
two interpreters simultaneously, and XNNPACK's demonstrably is not.

## Coral Support

libedgetpu is itself a TfLite delegate plugin, so an Edge TPU can be attached like
any other delegate -- which means it composes with `set_num_threads/2` and with
whatever else is on the builder:

```erlang
{ok, Delegate} = tflite_beam_coral:edge_tpu_delegate(),
ok = tflite_beam_interpreter_builder:add_delegate(Builder, Delegate),
ok = tflite_beam_interpreter_builder:build(Builder, Interpreter).
```

`tflite_beam_coral:make_edge_tpu_interpreter/2` still works and is unchanged. It
builds its own interpreter internally, though, so nothing set on a builder reaches
it; the delegate above is the composable route. Asking for a device that is not
there is an ordinary `{error, Reason}` from `edge_tpu_delegate/1`.

Both routes have been checked to produce identical output on a USB Coral
accelerator, running `mobilenet_v2_1.0_224_inat_bird_quant_edgetpu.tflite` against
libedgetpu 0.1.14 on macOS arm64.

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

Add `tflite_beam` to your list of dependencies in `rebar.config`:

```erlang
{deps, [
  {tflite_beam, "0.3.12"}
]}
```

Documentation is published on [HexDocs](https://hexdocs.pm/tflite_beam).

## Tests

```shell
rebar3 ct
```

The model fixtures live in `test/models/`, so the suite needs no network and runs
against a precompiled install as well as a build from source.

## Releasing

The precompiled tarballs only exist once the `v*` tag has been pushed and the
precompile matrix has finished, and the manifest that verifies them has to be
inside the published package -- so it is generated in between:

```shell
git tag -a vX.Y.Z -m "vX.Y.Z" && git push origin vX.Y.Z   # matrix builds the 7 targets
scripts/generate_checksums.sh X.Y.Z                        # writes checksum.term
git add checksum.term && git commit && git push
rebar3 hex publish
```

Skipping the middle step publishes a package that cannot check what it downloads,
which it will say out loud on install rather than doing quietly.

## Upstream Dependencies

- [cocoa-xu/libedgetpu](https://github.com/cocoa-xu/libedgetpu)

