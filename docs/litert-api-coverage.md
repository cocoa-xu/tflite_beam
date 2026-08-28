# What of LiteRT's C API this binds, and what it does not

LiteRT 2.2.0 publishes 27 headers under `litert/c`. This is which of them
tflite_beam reaches, and for the rest, the reason it does not. It exists so the
next person asking "did we miss something" gets an answer rather than a survey.

Everything here is behind `TFLITE_BEAM_ENABLE_LITERT_API=ON`.

## Bound

| header | reached through |
|---|---|
| `litert_common.h` | status strings, accelerator and precision enums |
| `litert_environment.h`, `litert_environment_options.h` | `tflite_beam_litert_compiled_model:environment/1`, which is how an accelerator plugin is found |
| `litert_model.h` | signature listing, subgraph input and output counts |
| `litert_options.h`, `litert_opaque_options.h` | accelerator selection, and the TOML payloads that carry precision and profiling |
| `litert_compiled_model.h` | `new/3`, `run/2`, `fully_accelerated/1`, `io_sizes/1` |
| `litert_profiler.h`, `litert_profiler_event.h` | `profile/1`, `summarise_profile/1`, `reset_profile/1` |
| `litert_metrics.h` | `run_with_metrics/2,3`, which brackets an inference and returns an empty list on every accelerator reachable here |
| `litert_tensor_buffer.h`, `litert_tensor_buffer_requirements.h`, `litert_tensor_buffer_types.h` | managed buffers, allocated by LiteRT from each tensor's requirements, which need not be host memory |
| `litert_platform_support.h` | `platform_support/0` |
| `litert_layout.h`, `litert_model_types.h`, `litert_any.h` | types the above pass around |

## Not bound, and why

**`litert_event.h`** builds events out of sync fence descriptors, OpenCL events
and EGL fences, for running a model without waiting for it. Every buffer here is
copied in and out synchronously inside one call, so there is nothing to wait on
that a `gen_server:call/3` does not already cover. It becomes worth binding if buffers
ever stay resident on a device between calls, and not before.

**`litert_op_options.h`** is 132 functions for reading one operator's options
out of a model. It answers questions about a graph, and nothing in tflite_beam
exposes a graph to ask them of. Binding it would mean first exposing operators
as things a caller can hold.

**`litert_builder.h`** builds and edits models programmatically. tflite_beam
runs models, it does not author them.

**`litert_custom_op_kernel.h`** lets a custom operator be implemented in the
host language. On the BEAM that means calling back into Erlang from inside an
inference, on a dirty scheduler, per operator invocation. That is a design with
real hazards and no demand yet.

**`litert_gl_types.h`, `litert_opencl_types.h`, `litert_webgpu_types.h`,
`litert_custom_tensor_buffer.h`** describe device-resident buffers. Same reason
as `litert_event.h`: nothing here holds one.

**`litert_event_type.h`, `litert_op_code.h`** are enumerations used by the
above.

## Cancellation, and why there is no timeout on a run

`litert_compiled_model.h` offers `LiteRtSetCompiledModelCancellationFunction`,
and it looks like the answer to an inference that never returns: a NIF cannot be
interrupted from Erlang, so a run that hangs holds a dirty scheduler, the calling
process and the model for the life of the VM.

It was built and measured, and it is not the answer. TFLite consults the callback
once before each node of the execution plan (`tflite/core/subgraph.cc:1761`), and
an accelerator that takes the whole graph leaves one node. Counted through an
instrumented callback: `multi_add.bin`, `add.bin` and `conv3d_huge_im2col.bin`
each consult it exactly **twice per inference**, whatever their size, and
`conv3d_huge_im2col.bin` runs for 709ms between those two points. A one
millisecond deadline does not stop it.

So the deadline can only be observed at the very start of a run. Shipping a
`run/3` whose timeout does not time out would be worse than not shipping one,
because a caller would rely on it, so there is no timeout on the direct API. The
server's `run/3` timeout is a `gen_server:call/3` timeout and says so: it ends
the wait, not the work.

Containing a hung or crashing inference needs the model in a separate OS process
that can be killed. That is a change to what this library is, not a flag, and it
would apply to `tflite_beam_interpreter` just as much.

## The one thing worth rechecking on an upstream bump

`litert_metrics.h` is bound but empty in practice, because filling it is an
accelerator's job through two entries of `LiteRtAcceleratorDef` that may be
null. Both the plugins in `tflite_delegate_plugins` and Google's own prebuilt
GPU accelerator leave them null. If a vendor accelerator ever appears that fills
them, `run_with_metrics/2` starts returning something without any change here.
