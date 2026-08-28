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

## LiteRT compiled models, and seeing where the time goes

Built only with `TFLITE_BEAM_ENABLE_LITERT_API=ON`, so the calls below raise
rather than return an error on a library without it.

A LiteRT compiled model is a second way to run a model, beside the interpreter.
It is not a faster one. Measured on an M4 Max against `mobilenet_v2_1.0_224`,
50 runs each, an interpreter with a GPU plugin attached and a compiled model on
the GPU land in the same band, because underneath they are the same delegate:

| | us/run |
|---|---|
| interpreter, CPU | 2205 to 2272 |
| interpreter + GPU plugin | 772 to 943 |
| compiled model, CPU | 2186 to 2212 |
| compiled model, GPU | 1076 to 1394 |

The GPU rows do not overlap: over three runs the interpreter with a plugin was
the faster of the two. Both reach the GPU through the same delegate, so read
this as "a compiled model buys no speed" rather than as a ranking of two
different engines.

What it has and the interpreter does not is a profiler. It reports every
operator, how long it took, and which of them an accelerator claimed:

```erlang
{ok, Env}   = tflite_beam_litert_compiled_model:environment("/opt/lib"),
{ok, Model} = tflite_beam_litert_compiled_model:new(Env, "model.tflite",
                  #{accelerators => [gpu], precision => fp32, profile => true}),
{ok, [Out]} = tflite_beam_litert_compiled_model:run(Model, [Input]),
{ok, Slow}  = tflite_beam_litert_compiled_model:summarise_profile(Model).
```

On the CPU `summarise_profile/1` names XNNPACK's kernels, and on the GPU it
shows the whole graph collapsed into one delegate node. LiteRT's own buffer
handling is not an operator and is not in that summary; it is in `profile/1`,
and is shown here beside them because it is what the overhead looks like:

```erlang
%% summarise_profile/1, on the CPU
{<<"Convolution (NHWC, F32) DWConv">>,  delegate_profiled, 357, 23831}
{<<"Fully Connected (NC, PF32) GEMM">>, delegate_profiled, 735, 18527}

%% and on the GPU
{<<"TfLiteMetalDelegate">>,             operator,           21, 24722}
```

The profile shapes are **provisional**: `summarise_profile/1` returns positional
tuples where named maps belong, `profile/1` passes LiteRT's enumeration numbers
through unnamed, and `run_with_metrics/2` types its values as `term()` for want
of a backend that fills them in. Everything else here, the constructors, the
binary input and output, the accelerator, precision and signature options, the
refusal semantics, `io_sizes/1` and `fully_accelerated/1`, is meant to be stable.

`Kind` is in the tuple because the categories can nest: a `delegate_operator`
runs inside a delegate and its time may already be counted in the enclosing
`delegate_profiled` entry. Totals within one kind add up; totals across kinds do
not. LiteRT's own buffer handling, which cost 197 and 96 microseconds over those
21 runs, is not an operator and is in `profile/1` rather than the summary.

Profiling cost at most 1.05x on the CPU here and nothing measurable on the GPU,
where the whole graph is one delegate node and there is no per-operator boundary
left to time. A graph an accelerator splits into many nodes has many more
boundaries, so measure your own.

The directory handed to `environment/1` is where LiteRT looks for a GPU
accelerator plugin. Without one, asking for `[gpu]` fails rather than quietly
running on the CPU. [`tflite_delegate_plugins`][plugins] builds plugins that
answer both this and the delegate interface above from one file.

[plugins]: https://github.com/cocoa-xu/tflite_delegate_plugins

### One model, one process

A compiled model owns one set of input and output buffers for its whole life,
and LiteRT does not promise its compiled model API is safe to enter from two
threads at once. So a second caller arriving while one is inside the model is
**refused**:

```erlang
{error, <<"compiled model is in use by another caller">>}
```

That is honest but it is not a queue. `tflite_beam_litert_compiled_model_server`
is the queue: it holds the model in one process, so callers wait their turn
instead of being told to come back.

```erlang
{ok, Server} = tflite_beam_litert_compiled_model_server:start_link(Env, "model.tflite",
                   #{accelerators => [gpu]}),
{ok, Outputs} = tflite_beam_litert_compiled_model_server:run(Server, [Input]).
```

Same split as `tflite_beam_interpreter` and `tflite_beam_interpreter_server`:
the direct module stays exactly as it is for callers who would rather serialise
access themselves.

### Or on a node of its own

Both of those run LiteRT inside the emulator, which is fast and is the right
default. It is also unconditional: a NIF cannot be interrupted, so a
segmentation fault in an accelerator plugin, a delegate that aborts, or an
inference that never returns takes the whole virtual machine with it, along with
every other model and every process that had nothing to do with it.

Whether that is acceptable depends on where the model came from, so it is a
choice rather than a decision made here.
`tflite_beam_litert_compiled_model_isolated` starts a second Erlang node, builds
the model there and forwards calls to it:

```erlang
{ok, Server} = tflite_beam_litert_compiled_model_isolated:start_link(
                   #{model_path => "model.tflite", accelerators => [cpu]}),
{ok, Outputs} = tflite_beam_litert_compiled_model_isolated:run(Server, [Input]).
```

Kill that node and the call returns `{error, Binary}`, this VM carries on, and a
supervisor starts another. What it costs: inputs and outputs are copied between
nodes twice per call, starting a node took about 200ms here, and the emulator
has to be distributed, which `start_link/1` arranges if nothing else has.

The server claims the model with
`tflite_beam_litert_compiled_model:controlling_process/2`, so the promise is
enforced rather than conventional: a `with/2` callback that keeps the reference
and uses it from somewhere else afterwards is refused. Claiming is opt-in and
available to anyone building their own owner, and a claim whose process has died
is released rather than stranding the model.

Before that refusal existed, four processes running twenty-five inferences each
against one shared model, each checking the answer to its own input, got a
handful of answers belonging to a different process with nothing to say which
ones. That is what the refusal is for.

## Threading

An interpreter, and any delegate attached to it, belongs to one process at a time.
TfLite documents `tflite::Interpreter` as not thread-safe and leaves serialising
access to the caller, and `invoke/1` runs on a dirty scheduler, so two processes
sharing one interpreter really do run it on two OS threads at once.

The direct API mirrors the C API, which means feeding an interpreter, running it
and reading the result back are three separate calls -- and nothing in the C API
says they have to be treated as one. Two processes taking turns badly get each
other's answers: measured on a real model, 147 wrong results in 400 calls,
silently and without a crash.

The guard described below now refuses most of those, and `predict/2` no longer
reads the output tensors after an invoke it was refused. What is left is the gap
between the three calls, which no per-call guard can close: the same measurement
today gives 6 wrong answers in 400 rather than 147. Six is not zero, so if more
than one process touches an interpreter, use the server.

**If you want that handled for you, use `tflite_beam_interpreter_server`:**

```erlang
{ok, Server} = tflite_beam_interpreter_server:start_link(ModelPath),
Output = tflite_beam_interpreter_server:predict(Server, [Input]).
```

The interpreter lives inside that process, so feeding, running and reading back is
one step nothing can interleave with, and concurrent callers each get the answer to
their own input. Use `with/2` for the sequences `predict/2` does not cover.

The direct API is unchanged and stays available. Two things guard it:

- Calls that genuinely overlap in time are refused rather than allowed to race.
  This is always on and costs a `trylock` on the uncontended path.
- `tflite_beam_interpreter:controlling_process/2` gives an interpreter to one
  process, after which every other process is refused whether it overlaps or not.
  It follows `gen_tcp:controlling_process/2`: while an interpreter belongs to
  nobody any process may take it, and once it belongs to someone only that process
  may hand it on. A controlling process that dies releases it.

Delegates are the same story: nothing documents a `TfLiteDelegate` as safe to back
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

The 1.0.0 release candidates carry the memory-safety work described above and
build the runtime from LiteRT rather than from TensorFlow. Hex never resolves a
pre-release from a range, so name it exactly:

```erlang
{deps, [
  {tflite_beam, "1.0.0-rc1"}
]}
```

`~> 0.3` will not reach it, which is deliberate: a two part 0.x requirement means
everything below 1.0.0, so releasing this work as 0.4.0 would have moved every
existing user onto a different upstream without their asking. Nothing in the
Erlang API was removed or renamed on the way, and the precompiled binaries ask
for exactly the glibc they asked for in 0.3.12.

Three things behave differently, each turning something silent into something you
can see. A tensor handle stops working once `allocate_tensors/1`, a resize or a
second `build/2` has moved what it points at, instead of reading memory that has
been given to something else; fetch it again afterwards. Writing to a tensor takes
exactly its size, where a short binary used to be written as far as it went and
reported as success, leaving the rest of the tensor holding whatever was there
before. And `tflite_version/0` now answers LiteRT's version rather than
TensorFlow's, which matters if you load a delegate plugin: it has to be built from
the same release, and the two version lines are not comparable, so LiteRT's 2.2.0
is newer than TensorFlow's 2.21.0 rather than older.

Documentation is published on [HexDocs](https://hexdocs.pm/tflite_beam).

### What the precompiled binaries need

Installing pulls a precompiled shared object rather than building one, so what
matters is what that object was linked against, not what your machine could
compile. These have held since v0.3.12:

| Target | Needs |
| --- | --- |
| `x86_64-linux-gnu` | glibc 2.29 (Ubuntu 20.04, Debian 11) |
| `armv7l-linux-gnueabihf` | glibc 2.29 (Raspberry Pi OS Bullseye) |
| `aarch64-linux-gnu` | glibc 2.34 (Ubuntu 22.04, Debian 12) |
| `armv6-linux-gnueabihf` | glibc 2.38 (Debian 13, or a Nerves system) |
| `riscv64-linux-gnu` | glibc 2.38 (Debian 13, or a Nerves system) |
| `aarch64-apple-darwin` | macOS 14 |
| `x86_64-apple-darwin` | macOS 15 |

The figure is a floor, not a pin: anything newer works. Check yours with
`ldd --version`, or read it off a downloaded object with
`readelf -V priv/tflite_beam.so | grep -o 'GLIBC_[0-9.]*' | sort -uV | tail -1`.

`armv6` and `riscv64` sit higher than the rest because they are built with the
Nerves toolchains, which carry their own glibc. That suits a Nerves system, which
ships a matching one. It does not suit Raspberry Pi OS Bookworm, which has 2.36,
so on that combination build from source:

```
export TFLITE_BEAM_PREFER_PRECOMPILED=false
```

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
rebar3 hex publish
```

`checksum.term` is not tracked in git and does not need to be: it is packaged from
the working directory, and the tarballs it lists do not exist until the tag has
been built -- so a tracked copy would always be one release out of date.

Skipping the middle step publishes a package that cannot check what it downloads,
which it says out loud on install rather than doing quietly.

## Upstream Dependencies

- [cocoa-xu/libedgetpu](https://github.com/cocoa-xu/libedgetpu)

