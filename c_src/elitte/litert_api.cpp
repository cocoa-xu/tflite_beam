#ifdef TFLITE_BEAM_LITERT_API_ENABLED
#include <erl_nif.h>

#include "../nif_utils.hpp"
#include "litert/c/litert_common.h"
#include "litert/c/litert_tensor_buffer_types.h"
#include "litert/c/litert_tensor_buffer.h"
#include "litert/c/litert_model_types.h"
#include <cstdlib>
#include <cstring>
#include <string>
#include <vector>
#include "litert/c/litert_compiled_model.h"
#include "litert/c/litert_model.h"
#include "litert/c/litert_options.h"
#include "litert/c/litert_environment.h"
#include "litert/c/litert_environment_options.h"
#include "litert/c/litert_opaque_options.h"
#include "litert/c/litert_tensor_buffer_requirements.h"

// The first thing here calls into LiteRT's own C API rather than the tflite one
// underneath it. It exists to prove the two are linked together and callable:
// a static library nothing references is dropped by the linker, so a build that
// merely compiles says nothing about whether the API is reachable.
ERL_NIF_TERM litert_api_status_string(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    int status;
    if (!enif_get_int(env, argv[0], &status)) {
        return erlang::nif::error(env, "expecting the status to be an integer");
    }

    const char * text = LiteRtGetStatusString(static_cast<LiteRtStatus>(status));
    if (text == nullptr) {
        return erlang::nif::error(env, "LiteRT gave no text for that status");
    }
    return erlang::nif::ok(env, erlang::nif::make_binary(env, text));
}

// Zero copy means handing LiteRT the bytes a caller already has. LiteRT wants
// them 64 byte aligned (LITERT_HOST_MEMORY_BUFFER_ALIGNMENT), and nothing
// promises that an Erlang binary is. This reports what the alignment actually
// is, so the question is settled by measurement rather than by assumption.
ERL_NIF_TERM litert_api_binary_alignment(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return erlang::nif::error(env, "expecting a binary");
    }

    uintptr_t addr = reinterpret_cast<uintptr_t>(bin.data);
    unsigned alignment = 1;
    while (alignment < 4096 && (addr % (alignment * 2)) == 0) alignment *= 2;

    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map, erlang::nif::atom(env, "size"),
                      enif_make_uint64(env, bin.size), &map);
    enif_make_map_put(env, map, erlang::nif::atom(env, "alignment"),
                      enif_make_uint(env, alignment), &map);
    enif_make_map_put(env, map, erlang::nif::atom(env, "meets_litert_64"),
                      erlang::nif::atom(env, (addr % LITERT_HOST_MEMORY_BUFFER_ALIGNMENT) == 0 ? "true" : "false"), &map);
    return erlang::nif::ok(env, map);
}

static void litert_beam_free_aligned(void * addr) { free(addr); }

// A tensor buffer over memory this owns rather than over an Erlang binary.
// Binaries are not 64 byte aligned and cannot be asked to be, but nothing says
// the buffer handed to LiteRT has to be one: allocated aligned here, with the
// deallocator LiteRT calls when it is done, so ownership is stated rather than
// assumed. This exists to answer whether the path works at all before anything
// is built on it.
ERL_NIF_TERM litert_api_host_buffer_roundtrip(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin)) {
        return erlang::nif::error(env, "expecting a binary");
    }

    void * aligned = nullptr;
    size_t padded = ((bin.size + LITERT_HOST_MEMORY_BUFFER_ALIGNMENT - 1) /
                     LITERT_HOST_MEMORY_BUFFER_ALIGNMENT) * LITERT_HOST_MEMORY_BUFFER_ALIGNMENT;
    if (posix_memalign(&aligned, LITERT_HOST_MEMORY_BUFFER_ALIGNMENT, padded) != 0 || aligned == nullptr) {
        return erlang::nif::error(env, "cannot allocate an aligned host buffer");
    }
    memcpy(aligned, bin.data, bin.size);

    int32_t dims[1] = { static_cast<int32_t>(bin.size) };
    LiteRtRankedTensorType type;
    memset(&type, 0, sizeof(type));
    type.element_type = kLiteRtElementTypeUInt8;
    type.layout.rank = 1;
    type.layout.dimensions[0] = dims[0];

    LiteRtTensorBuffer buffer = nullptr;
    LiteRtStatus st = LiteRtCreateTensorBufferFromHostMemory(
        &type, aligned, bin.size, litert_beam_free_aligned, &buffer);
    if (st != kLiteRtStatusOk) {
        free(aligned);
        return erlang::nif::error(env, LiteRtGetStatusString(st));
    }

    void * back = nullptr;
    st = LiteRtGetTensorBufferHostMemory(buffer, &back);
    bool same = (st == kLiteRtStatusOk) && (back == aligned);
    bool bytes_match = same && (memcmp(back, bin.data, bin.size) == 0);

    LiteRtDestroyTensorBuffer(buffer);

    ERL_NIF_TERM map = enif_make_new_map(env);
    enif_make_map_put(env, map, erlang::nif::atom(env, "created"),
                      erlang::nif::atom(env, "true"), &map);
    enif_make_map_put(env, map, erlang::nif::atom(env, "same_pointer_back"),
                      erlang::nif::atom(env, same ? "true" : "false"), &map);
    enif_make_map_put(env, map, erlang::nif::atom(env, "bytes_match"),
                      erlang::nif::atom(env, bytes_match ? "true" : "false"), &map);
    return erlang::nif::ok(env, map);
}
// appended into litert_api.cpp
static void cm_free(void * addr) { free(addr); }

struct CompiledModelProbe {
    LiteRtEnvironment env = nullptr;
    LiteRtModel model = nullptr;
    LiteRtOptions options = nullptr;
    LiteRtCompiledModel compiled = nullptr;
    std::vector<LiteRtTensorBuffer> ins, outs;
    std::vector<void *> in_mem, out_mem;
    std::vector<size_t> in_size, out_size;
    ~CompiledModelProbe() {
        for (auto b : ins) if (b) LiteRtDestroyTensorBuffer(b);
        for (auto b : outs) if (b) LiteRtDestroyTensorBuffer(b);
        if (compiled) LiteRtDestroyCompiledModel(compiled);
        if (options) LiteRtDestroyOptions(options);
        if (model) LiteRtDestroyModel(model);
        if (env) LiteRtDestroyEnvironment(env);
    }
};

// Builds a compiled model, allocates one aligned buffer per input and output to
// the size LiteRT asks for, then runs it however many times were requested,
// reusing those buffers. The point is the reuse: binding happens once and each
// run writes into memory that is already where the runtime wants it.
ERL_NIF_TERM litert_api_compiled_model_bench(ErlNifEnv *env_nif, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 5) return enif_make_badarg(env_nif);

    std::string path;
    int iters;
    if (!erlang::nif::get(env_nif, argv[0], path) || !enif_get_int(env_nif, argv[1], &iters)) {
        return erlang::nif::error(env_nif, "expecting a path and an iteration count");
    }

    CompiledModelProbe p;
    LiteRtStatus st;
#define CHECK(expr, what) st = (expr); if (st != kLiteRtStatusOk) { \
        return erlang::nif::error(env_nif, (std::string(what) + ": " + LiteRtGetStatusString(st)).c_str()); }

    // the GPU accelerator is a plugin dlopen'd at run time; with no runtime
    // library dir it is searched for relative to the empty path and never found
    std::string accel_dir;
    LiteRtEnvOption env_opts[1];
    int num_env_opts = 0;
    if (argc > 3 && erlang::nif::get(env_nif, argv[3], accel_dir) && !accel_dir.empty()) {
        env_opts[0].tag = kLiteRtEnvOptionTagRuntimeLibraryDir;
        env_opts[0].value.type = kLiteRtAnyTypeString;
        env_opts[0].value.str_value = accel_dir.c_str();
        num_env_opts = 1;
    }
    CHECK(LiteRtCreateEnvironment(num_env_opts, num_env_opts ? env_opts : nullptr, &p.env), "create environment")
    CHECK(LiteRtCreateModelFromFile(p.env, path.c_str(), &p.model), "load model")
    CHECK(LiteRtCreateOptions(&p.options), "create options")
    // without this the compile is refused with InvalidArgument: an options object
    // carries no accelerator by default and none is not a valid answer
    int accel_flag;
    if (!enif_get_int(env_nif, argv[2], &accel_flag)) {
        return erlang::nif::error(env_nif, "expecting the accelerator set to be an integer");
    }
    CHECK(LiteRtSetOptionsHardwareAccelerators(p.options, (LiteRtHwAcceleratorSet)accel_flag), "select accelerator")

    // an accelerator reads its settings from a TOML payload filed under its own
    // identifier, so asking a GPU accelerator for a precision is a matter of
    // attaching one; zero leaves the accelerator on its own default
    int precision;
    if (!enif_get_int(env_nif, argv[4], &precision)) {
        return erlang::nif::error(env_nif, "expecting the precision to be an integer");
    }
    if (precision != 0) {
        char toml[32];
        snprintf(toml, sizeof(toml), "precision = %d\n", precision);
        char *payload = strdup(toml);
        if (payload == nullptr) return erlang::nif::error(env_nif, "out of memory");
        LiteRtOpaqueOptions gpu_options = nullptr;
        st = LiteRtCreateOpaqueOptions("gpu_options", payload,
                                       [](void *d) { free(d); }, &gpu_options);
        if (st != kLiteRtStatusOk) { free(payload); return erlang::nif::error(env_nif, "gpu options"); }
        CHECK(LiteRtAddOpaqueOptions(p.options, gpu_options), "attach gpu options")
    }
    CHECK(LiteRtCreateCompiledModel(p.env, p.model, p.options, &p.compiled), "compile model")

    auto make_buffers = [&](bool input, size_t count) -> const char * {
        for (size_t i = 0; i < count; i++) {
            LiteRtTensorBufferRequirements req = nullptr;
            LiteRtStatus s = input
                ? LiteRtGetCompiledModelInputBufferRequirements(p.compiled, 0, i, &req)
                : LiteRtGetCompiledModelOutputBufferRequirements(p.compiled, 0, i, &req);
            if (s != kLiteRtStatusOk) return input ? "input requirements" : "output requirements";
            size_t bytes = 0;
            if (LiteRtGetTensorBufferRequirementsBufferSize(req, &bytes) != kLiteRtStatusOk) return "buffer size";

            void * mem = nullptr;
            size_t padded = ((bytes + 63) / 64) * 64;
            if (posix_memalign(&mem, 64, padded) != 0) return "aligned allocation";
            memset(mem, 0, padded);

            // Ask for the layout rather than inventing one. A buffer declared as a
            // flat run of bytes compiles and then fails at run time: the model
            // expects its own shape, and the buffer has to say the same thing.
            LiteRtLayout layout;
            memset(&layout, 0, sizeof(layout));
            if (input) {
                if (LiteRtGetCompiledModelInputTensorLayout(p.compiled, 0, i, &layout) != kLiteRtStatusOk)
                    return "input tensor layout";
            } else {
                LiteRtLayout layouts[8];
                if (LiteRtGetCompiledModelOutputTensorLayouts(p.compiled, 0, count, layouts, false) != kLiteRtStatusOk)
                    return "output tensor layouts";
                layout = layouts[i];
            }

            LiteRtRankedTensorType type;
            memset(&type, 0, sizeof(type));
            type.element_type = kLiteRtElementTypeUInt8;
            type.layout = layout;

            LiteRtTensorBuffer buf = nullptr;
            if (LiteRtCreateTensorBufferFromHostMemory(&type, mem, bytes, cm_free, &buf) != kLiteRtStatusOk) {
                free(mem); return "tensor buffer";
            }
            if (input)  { p.ins.push_back(buf);  p.in_mem.push_back(mem);  p.in_size.push_back(bytes); }
            else        { p.outs.push_back(buf); p.out_mem.push_back(mem); p.out_size.push_back(bytes); }
        }
        return nullptr;
    };
    if (const char * why = make_buffers(true, 1))  return erlang::nif::error(env_nif, why);
    if (const char * why = make_buffers(false, 1)) return erlang::nif::error(env_nif, why);

    CHECK(LiteRtRunCompiledModel(p.compiled, 0, p.ins.size(), p.ins.data(), p.outs.size(), p.outs.data()), "warm up run")

    ErlNifTime t0 = enif_monotonic_time(ERL_NIF_USEC);
    for (int i = 0; i < iters; i++) {
        memset(p.in_mem[0], 128, p.in_size[0]);
        st = LiteRtRunCompiledModel(p.compiled, 0, p.ins.size(), p.ins.data(), p.outs.size(), p.outs.data());
        if (st != kLiteRtStatusOk) return erlang::nif::error(env_nif, "run failed partway");
    }
    ErlNifTime t1 = enif_monotonic_time(ERL_NIF_USEC);
#undef CHECK

    ERL_NIF_TERM map = enif_make_new_map(env_nif);
    enif_make_map_put(env_nif, map, erlang::nif::atom(env_nif, "us_per_run"),
                      enif_make_double(env_nif, (double)(t1 - t0) / iters), &map);
    enif_make_map_put(env_nif, map, erlang::nif::atom(env_nif, "input_bytes"),
                      enif_make_uint64(env_nif, p.in_size[0]), &map);
    enif_make_map_put(env_nif, map, erlang::nif::atom(env_nif, "output_bytes"),
                      enif_make_uint64(env_nif, p.out_size[0]), &map);
    // the input is a constant memset, so the output is deterministic and
    // comparable across accelerators: a speedup that changes the answer is not one
    ERL_NIF_TERM out_bin;
    unsigned char *out_buf = enif_make_new_binary(env_nif, p.out_size[0], &out_bin);
    memcpy(out_buf, p.out_mem[0], p.out_size[0]);
    enif_make_map_put(env_nif, map, erlang::nif::atom(env_nif, "output"), out_bin, &map);
    bool fully = false;
    LiteRtCompiledModelIsFullyAccelerated(p.compiled, &fully);
    enif_make_map_put(env_nif, map, erlang::nif::atom(env_nif, "fully_accelerated"),
                      erlang::nif::atom(env_nif, fully ? "true" : "false"), &map);
    return erlang::nif::ok(env_nif, map);
}
#endif
