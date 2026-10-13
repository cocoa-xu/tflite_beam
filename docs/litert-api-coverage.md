# What of LiteRT's C API this binds, and what it does not

LiteRT 2.3.0 publishes 28 headers under `litert/c`. This is which of them
tflite_beam reaches, and for the rest, the reason it does not. It exists so the
next person asking "did we miss something" gets an answer rather than a survey.

Everything here is behind `TFLITE_BEAM_ENABLE_LITERT_API=ON`.

## Bound

| header | reached through |
|---|---|
| `litert_common.h` | status strings, accelerator and precision enums |
| `litert_environment.h`, `litert_environment_options.h` | `tflite_beam_litert_compiled_model:environment/1`, which is how an accelerator plugin is found |
| `litert_model.h` | signature listing, experimental graph inspection and serialization |
| `litert_builder.h`, `litert_op_options.h` | experimental OneHot, SplitV and StableHLO composite option rewrites |
| `litert_options.h`, `litert_opaque_options.h` | accelerator selection, and the TOML payloads that carry precision and profiling |
| `options/litert_{cpu,gpu,arm,intel_openvino,google_tensor,mediatek,qualcomm}_options.h` | typed backend settings, native serialization, weight sharing and CPU weight caches through `backend_options` |
| `litert_compiled_model.h` | `new/3`, `run/2`, `fully_accelerated/1`, `non_cpu_fully_accelerated/1`, `io_sizes/1` |
| `litert_profiler.h`, `litert_profiler_event.h` | `profile/1`, `summarise_profile/1`, `reset_profile/1` |
| `litert_metrics.h` | `run_with_metrics/2,3`, which brackets inference and uses `LiteRtAppendMetric` for optional host timing and extra metrics |
| `litert_tensor_buffer.h`, `litert_tensor_buffer_requirements.h`, `litert_tensor_buffer_types.h` | managed buffers, allocated by LiteRT from each tensor's requirements, which need not be host memory |
| `litert_platform_support.h` | `platform_support/0` |
| `litert_layout.h`, `litert_model_types.h`, `litert_any.h` | types the above pass around |

## Not bound, and why

**`litert_event.h`** builds events out of sync fence descriptors, OpenCL events
and EGL fences, for running a model without waiting for it. Every buffer here is
copied in and out synchronously inside one call, so there is nothing to wait on
that a `gen_server:call/3` does not already cover. It becomes worth binding if buffers
ever stay resident on a device between calls, and not before.

Most of **`litert_op_options.h`** remains unbound. Graph inspection reports
connectivity for every operator and options for OneHot, SplitV and StableHLO
composite. The three corresponding builder functions added in 2.3.0 are bound
through `tflite_beam_litert_model:rewrite/3`. It operates on a private model and
returns a serialized binary. LiteRT exposes no public standalone builder
creation or commit API, so the binding uses the pinned runtime's internal
builder and serializer. It preserves the graph's topology and is experimental.

**`litert_custom_op_kernel.h`** lets a custom operator be implemented in the
host language. On the BEAM that means calling back into Erlang from inside an
inference, on a dirty scheduler, per operator invocation. That is a design with
real hazards and no demand yet.

**`litert_gl_types.h`, `litert_opencl_types.h`, `litert_webgpu_types.h`,
`litert_custom_tensor_buffer.h`** describe device-resident buffers. Same reason
as `litert_event.h`: nothing here holds one.

**`litert_event_type.h`, `litert_op_code.h`** are enumerations used by the
above.

**`litert_profiler_types.h`** defines native tracing callbacks. The binding
reads recorded profiling events synchronously and does not install callbacks.

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

Accelerator metrics depend on two optional entries of `LiteRtAcceleratorDef`.
LiteRT 2.3.0's ML Drift GPU implementation supplies
`gpu_intermediate_memory_bytes` and `gpu_constant_memory_bytes` when its backend
can measure them. Other plugins may still supply none. The binding preserves
their names and values, and appends optional host timing and caller metrics
after them. New accelerator counters need no additional Erlang API.
