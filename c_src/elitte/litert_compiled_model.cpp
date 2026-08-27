// LiteRT's compiled model, exposed as it is rather than wrapped.
//
// The reason to have this at all is not speed. Measured on the same model and
// machine, a compiled model on the GPU was no faster than an interpreter with
// the same plugin attached as an external delegate, and in three runs it was
// slower: 1076 to 1394 microseconds against 772 to 943. Underneath they are the
// same delegate. What only exists here is the profiler:
// LiteRtCompiledModelGetProfiler has no counterpart on an interpreter, and it
// is the only way to see, operator by operator, where the time went and which
// operators an accelerator actually claimed.
#ifdef TFLITE_BEAM_LITERT_API_ENABLED
#include <erl_nif.h>

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <string>
#include <vector>

#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"

#include "litert/c/litert_common.h"
#include "litert/c/litert_compiled_model.h"
#include "litert/c/litert_environment.h"
#include "litert/c/litert_environment_options.h"
#include "litert/c/litert_model.h"
#include "litert/c/litert_opaque_options.h"
#include "litert/c/litert_options.h"
#include "litert/c/litert_profiler.h"
#include "litert/c/litert_profiler_event.h"
#include "litert/c/litert_tensor_buffer.h"
#include "litert/c/litert_tensor_buffer_requirements.h"
#include "litert/c/litert_metrics.h"
#include "litert/c/litert_platform_support.h"

namespace {

ERL_NIF_TERM litert_error(ErlNifEnv *env, const char *what, LiteRtStatus st) {
    return erlang::nif::error(env, (std::string(what) + ": " + LiteRtGetStatusString(st)).c_str());
}

// Resolving a compiled model for use: the handle, then the lock, then the
// ownership check under it, and the lock is held until this goes out of scope.
// Trying rather than waiting, because a NIF that blocks blocks a scheduler, and
// because a caller who has been told the model is busy can decide what to do
// about it; waiting would only hide that two processes are sharing one.
class CompiledModelUse {
public:
    CompiledModelUse(ErlNifEnv *env, ERL_NIF_TERM term) : env_(env) {
        res_ = NifResLiteRtCompiledModel::get_resource(env, term, error_);
        if (res_ == nullptr) return;

        held_ = std::unique_ptr<MutexTryLock>(new MutexTryLock(res_->lock));
        if (!held_->acquired()) {
            res_ = nullptr;
            error_ = erlang::nif::error(env, "compiled model is in use by another caller");
            return;
        }
        if (!compiled_model_caller_may_use(env, res_)) {
            res_ = nullptr;
            error_ = erlang::nif::error(env, "compiled model belongs to another process");
        }
    }

    explicit operator bool() const { return res_ != nullptr; }
    NifResLiteRtCompiledModel * operator->() const { return res_; }
    NifResLiteRtCompiledModel * get() const { return res_; }
    ERL_NIF_TERM error() const { return error_; }

private:
    ErlNifEnv *env_;
    NifResLiteRtCompiledModel *res_ = nullptr;
    ERL_NIF_TERM error_{};
    std::unique_ptr<MutexTryLock> held_;
};

// Handed to LiteRT for the TOML payloads this file strdups.
void release_malloced(void *addr) { free(addr); }

// Attaches one TOML payload under its identifier. LiteRT files an accelerator's
// settings this way, so both precision and profiling travel the same road.
LiteRtStatus add_toml_options(LiteRtOptions options, const char *identifier, const char *toml) {
    char *payload = strdup(toml);
    if (payload == nullptr) return kLiteRtStatusErrorMemoryAllocationFailure;
    LiteRtOpaqueOptions opaque = nullptr;
    LiteRtStatus st = LiteRtCreateOpaqueOptions(identifier, payload, release_malloced, &opaque);
    // the payload is only taken on success, so freeing it here is not a double free
    if (st != kLiteRtStatusOk) { free(payload); return st; }

    st = LiteRtAddOpaqueOptions(options, opaque);
    if (st != kLiteRtStatusOk) {
        // not appended, so it is still ours, and it owns the payload now
        LiteRtDestroyOpaqueOptions(opaque);
    }
    return st;
}

}  // namespace

// litert_environment_new(RuntimeLibraryDir) -> {ok, Env} | {error, Reason}
//
// The directory is where a GPU accelerator plugin is looked for. An empty one
// leaves LiteRT searching relative to nothing, which is why a GPU compile then
// fails with only a line in the log to say so.
ERL_NIF_TERM litert_environment_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    std::string dir;
    if (!erlang::nif::get(env, argv[0], dir)) {
        return erlang::nif::error(env, "expecting the runtime library directory to be a string");
    }

    ERL_NIF_TERM error{};
    auto res = NifResLiteRtEnvironment::allocate_resource(env, error);
    if (res == nullptr) return error;
    ResourceRef<NifResLiteRtEnvironment> hold(res);

    LiteRtEnvOption opts[1];
    int num_opts = 0;
    if (!dir.empty()) {
        opts[0].tag = kLiteRtEnvOptionTagRuntimeLibraryDir;
        opts[0].value.type = kLiteRtAnyTypeString;
        opts[0].value.str_value = dir.c_str();
        num_opts = 1;
    }

    LiteRtStatus st = LiteRtCreateEnvironment(num_opts, num_opts ? opts : nullptr, &res->val);
    if (st != kLiteRtStatusOk) return litert_error(env, "create environment", st);

    return erlang::nif::ok(env, enif_make_resource(env, res));
}

// litert_compiled_model_new(Env, Path, Opts) -> {ok, Model} | {error, Reason}
//
// Opts is a map: accelerators (an integer bitset), precision (a
// LiteRtDelegatePrecision), profile (0 or 1). Buffers for signature 0 are
// allocated once here rather than per run, which is what makes a compiled
// model something a caller has to serialise access to.
ERL_NIF_TERM litert_compiled_model_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 6) return enif_make_badarg(env);

    ERL_NIF_TERM error{};
    auto env_res = NifResLiteRtEnvironment::get_resource(env, argv[0], error);
    if (env_res == nullptr) return error;

    std::string path;
    int accelerators = 0, precision = 0, profile = 0, signature = 0;
    if (!erlang::nif::get(env, argv[1], path) ||
        !enif_get_int(env, argv[2], &accelerators) ||
        !enif_get_int(env, argv[3], &precision) ||
        !enif_get_int(env, argv[4], &profile) ||
        !enif_get_int(env, argv[5], &signature) || signature < 0) {
        return erlang::nif::error(env, "expecting a path and four integers");
    }

    auto res = NifResLiteRtCompiledModel::allocate_resource(env, error);
    if (res == nullptr) return error;
    ResourceRef<NifResLiteRtCompiledModel> hold(res);

    LiteRtStatus st;
#define TRY(expr, what) st = (expr); if (st != kLiteRtStatusOk) return litert_error(env, what, st);

    TRY(LiteRtCreateModelFromFile(env_res->val, path.c_str(), &res->model), "load model")
    TRY(LiteRtCreateOptions(&res->options), "create options")
    // an options object carries no accelerator by default, and none is not a
    // valid answer to LiteRtCreateCompiledModel
    TRY(LiteRtSetOptionsHardwareAccelerators(res->options, (LiteRtHwAcceleratorSet)accelerators),
        "select accelerator")

    if (precision != 0) {
        char toml[40];
        snprintf(toml, sizeof(toml), "precision = %d\n", precision);
        TRY(add_toml_options(res->options, "gpu_options", toml), "attach gpu options")
    }
    if (profile) {
        TRY(add_toml_options(res->options, "runtime_options_string", "enable_profiling = true\n"),
            "attach runtime options")
    }

    TRY(LiteRtCreateCompiledModel(env_res->val, res->model, res->options, &res->val), "compile model")

    // the environment is reachable from the compiled model as a plain pointer,
    // so it has to outlive it
    res->environment = env_res;
    enif_keep_resource(env_res);

    if (profile) {
        LiteRtProfiler prof = nullptr;
        if (LiteRtCompiledModelGetProfiler(res->val, &prof) == kLiteRtStatusOk && prof != nullptr) {
            res->profiler = prof;
            LiteRtStartProfiler(prof);
        }
    }

    res->inputs       = new std::vector<LiteRtTensorBuffer>();
    res->outputs      = new std::vector<LiteRtTensorBuffer>();
    res->input_sizes  = new std::vector<size_t>();
    res->output_sizes = new std::vector<size_t>();

    LiteRtParamIndex num_signatures = 0;
    if (LiteRtGetNumModelSignatures(res->model, &num_signatures) != kLiteRtStatusOk) {
        return erlang::nif::error(env, "cannot count the model's signatures");
    }
    if ((LiteRtParamIndex)signature >= num_signatures) {
        char msg[96];
        snprintf(msg, sizeof(msg), "this model has %llu signature%s, %d asked for",
                 (unsigned long long)num_signatures,
                 num_signatures == 1 ? "" : "s", signature);
        return erlang::nif::error(env, msg);
    }
    res->signature = (LiteRtParamIndex)signature;

    // A signature index is not a subgraph index: signatures may share a subgraph
    // and may name a subset of its tensors in their own order. Counting the
    // subgraph's tensors would allocate buffers that do not match the signature
    // then handed to LiteRtRunCompiledModel, so the signature is asked directly.
    LiteRtSignature sig = nullptr;
    LiteRtParamIndex n_in = 0, n_out = 0;
    if (LiteRtGetModelSignature(res->model, res->signature, &sig) != kLiteRtStatusOk ||
        LiteRtGetNumSignatureInputs(sig, &n_in) != kLiteRtStatusOk ||
        LiteRtGetNumSignatureOutputs(sig, &n_out) != kLiteRtStatusOk) {
        return erlang::nif::error(env, "cannot count the signature's inputs and outputs");
    }

    // Output layouts come back for every output at once, so they are fetched
    // before the loop rather than inside it.
    std::vector<LiteRtLayout> out_layouts(n_out ? n_out : 1);
    memset(out_layouts.data(), 0, out_layouts.size() * sizeof(LiteRtLayout));
    if (n_out > 0 &&
        LiteRtGetCompiledModelOutputTensorLayouts(res->val, res->signature, n_out, out_layouts.data(), false)
            != kLiteRtStatusOk) {
        return erlang::nif::error(env, "output tensor layouts");
    }

    auto make_buffers = [&](bool input, LiteRtParamIndex count) -> const char * {
        for (LiteRtParamIndex i = 0; i < count; i++) {
            LiteRtTensorBufferRequirements req = nullptr;
            LiteRtStatus s = input
                ? LiteRtGetCompiledModelInputBufferRequirements(res->val, res->signature, i, &req)
                : LiteRtGetCompiledModelOutputBufferRequirements(res->val, res->signature, i, &req);
            if (s != kLiteRtStatusOk) return input ? "input requirements" : "output requirements";

            size_t bytes = 0;
            if (LiteRtGetTensorBufferRequirementsBufferSize(req, &bytes) != kLiteRtStatusOk) {
                return "buffer size";
            }

            // Ask for the layout rather than inventing one. A buffer declared as
            // a flat run of bytes compiles and then fails at run time, because
            // the model expects its own shape and the buffer has to say so too.
            LiteRtLayout layout;
            memset(&layout, 0, sizeof(layout));
            if (input) {
                if (LiteRtGetCompiledModelInputTensorLayout(res->val, res->signature, i, &layout)
                    != kLiteRtStatusOk) {
                    return "input tensor layout";
                }
            } else {
                layout = out_layouts[i];
            }

            LiteRtRankedTensorType type;
            memset(&type, 0, sizeof(type));
            type.element_type = kLiteRtElementTypeUInt8;
            type.layout = layout;

            // LiteRT allocates this, not us. Hand-rolling it meant rounding the
            // size up to the alignment and hoping, while LiteRT's own allocator
            // adds XNN_EXTRA_BYTES of guard that XNNPACK kernels are allowed to
            // touch, and meant owning memory whose ownership on failure is
            // ambiguous: LiteRtCreateTensorBufferFromHostMemory installs the
            // deallocator before it validates, so a validation failure frees the
            // memory and freeing it again here was a double free.
            LiteRtTensorBuffer buf = nullptr;
            s = LiteRtCreateManagedTensorBufferFromRequirements(
                    env_res->val, &type, req, &buf);
            if (s != kLiteRtStatusOk || buf == nullptr) return "tensor buffer";

            // push_back can throw, and a buffer that never reached the vector
            // would be unreachable by the destructor, so the space is reserved
            // before any of them exist.
            if (input) {
                res->inputs->push_back(buf);
                res->input_sizes->push_back(bytes);
            } else {
                res->outputs->push_back(buf);
                res->output_sizes->push_back(bytes);
            }
        }
        return nullptr;
    };
    res->inputs->reserve(n_in);
    res->input_sizes->reserve(n_in);
    res->outputs->reserve(n_out);
    res->output_sizes->reserve(n_out);

    if (const char *why = make_buffers(true, n_in))   return erlang::nif::error(env, why);
    if (const char *why = make_buffers(false, n_out)) return erlang::nif::error(env, why);
#undef TRY

    return erlang::nif::ok(env, enif_make_resource(env, res));
}

// litert_compiled_model_run(Model, Inputs) -> {ok, [binary()]} | {error, Reason}
//
// Inputs is a list of binaries, one per input buffer and each exactly the size
// that buffer wants. They are copied in, the model runs, and the outputs are
// copied back out. Two processes doing this against the same model interleave
// on the buffers, which is what the server wrapper exists to prevent.
ERL_NIF_TERM litert_compiled_model_run(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    CompiledModelUse res(env, argv[0]);
    if (!res) return res.error();

    unsigned int given = 0;
    if (!enif_get_list_length(env, argv[1], &given)) {
        return erlang::nif::error(env, "expecting a list of input binaries");
    }
    if (given != res->inputs->size()) {
        char msg[96];
        snprintf(msg, sizeof(msg), "this model takes %zu inputs, %u given",
                 res->inputs->size(), given);
        return erlang::nif::error(env, msg);
    }

    ERL_NIF_TERM head, tail = argv[1];
    for (size_t i = 0; i < res->inputs->size(); i++) {
        if (!enif_get_list_cell(env, tail, &head, &tail)) {
            return erlang::nif::error(env, "expecting a list of input binaries");
        }
        ErlNifBinary bin;
        if (!enif_inspect_binary(env, head, &bin)) {
            return erlang::nif::error(env, "expecting every input to be a binary");
        }
        if (bin.size != res->input_sizes->at(i)) {
            char msg[112];
            snprintf(msg, sizeof(msg), "input %zu wants %zu bytes, %zu given",
                     i, res->input_sizes->at(i), bin.size);
            return erlang::nif::error(env, msg);
        }
        // Writing through Lock rather than at the pointer we allocated. For host
        // memory the two are the same address today, but the lock is where
        // LiteRT synchronises on any event attached to the buffer and where a
        // strided buffer is packed, and neither of those is our business to
        // assume away.
        void *addr = nullptr;
        LiteRtStatus ls = LiteRtLockTensorBuffer(res->inputs->at(i), &addr,
                                                 kLiteRtTensorBufferLockModeWrite);
        if (ls != kLiteRtStatusOk || addr == nullptr) {
            return litert_error(env, "lock input buffer", ls);
        }
        memcpy(addr, bin.data, bin.size);
        LiteRtUnlockTensorBuffer(res->inputs->at(i));
    }

    LiteRtStatus st = LiteRtRunCompiledModel(res->val, res->signature,
                                             res->inputs->size(), res->inputs->data(),
                                             res->outputs->size(), res->outputs->data());
    if (st != kLiteRtStatusOk) return litert_error(env, "run", st);

    ERL_NIF_TERM outputs = enif_make_list(env, 0);
    for (size_t i = res->outputs->size(); i-- > 0; ) {
        void *addr = nullptr;
        LiteRtStatus ls = LiteRtLockTensorBuffer(res->outputs->at(i), &addr,
                                                 kLiteRtTensorBufferLockModeRead);
        if (ls != kLiteRtStatusOk || addr == nullptr) {
            return litert_error(env, "lock output buffer", ls);
        }
        ERL_NIF_TERM bin_term;
        unsigned char *out = enif_make_new_binary(env, res->output_sizes->at(i), &bin_term);
        if (out == nullptr) {
            LiteRtUnlockTensorBuffer(res->outputs->at(i));
            return erlang::nif::error(env, "cannot allocate the output binary");
        }
        memcpy(out, addr, res->output_sizes->at(i));
        LiteRtUnlockTensorBuffer(res->outputs->at(i));
        outputs = enif_make_list_cell(env, bin_term, outputs);
    }
    return erlang::nif::ok(env, outputs);
}

// litert_compiled_model_fully_accelerated(Model) -> {ok, boolean()}
ERL_NIF_TERM litert_compiled_model_fully_accelerated(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);
    CompiledModelUse res(env, argv[0]);
    if (!res) return res.error();

    bool fully = false;
    LiteRtStatus st = LiteRtCompiledModelIsFullyAccelerated(res->val, &fully);
    if (st != kLiteRtStatusOk) return litert_error(env, "fully accelerated", st);
    return erlang::nif::ok(env, erlang::nif::atom(env, fully ? "true" : "false"));
}

// litert_compiled_model_profile(Model) -> {ok, [map()]} | {error, Reason}
//
// Empty unless the model was compiled with profiling on. Telemetry events carry
// a sentinel rather than a duration and are left in: deciding what is noise is
// the caller's, not this layer's.
ERL_NIF_TERM litert_compiled_model_profile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);
    CompiledModelUse res(env, argv[0]);
    if (!res) return res.error();

    ERL_NIF_TERM events = enif_make_list(env, 0);
    if (res->profiler == nullptr) return erlang::nif::ok(env, events);

    int n = 0;
    LiteRtStatus st = LiteRtGetNumProfilerEvents(res->profiler, &n);
    if (st != kLiteRtStatusOk) return litert_error(env, "profiler event count", st);
    if (n <= 0) return erlang::nif::ok(env, events);

    std::vector<ProfiledEventData> buf(n);
    st = LiteRtGetProfilerEvents(res->profiler, n, buf.data());
    if (st != kLiteRtStatusOk) return litert_error(env, "profiler events", st);

    for (int i = n - 1; i >= 0; i--) {
        ERL_NIF_TERM ev = enif_make_new_map(env);
        enif_make_map_put(env, ev, erlang::nif::atom(env, "tag"),
            erlang::nif::make_binary(env, buf[i].tag ? buf[i].tag : ""), &ev);
        enif_make_map_put(env, ev, erlang::nif::atom(env, "us"),
            enif_make_uint64(env, buf[i].elapsed_time_us), &ev);
        enif_make_map_put(env, ev, erlang::nif::atom(env, "type"),
            enif_make_int(env, (int)buf[i].event_type), &ev);
        enif_make_map_put(env, ev, erlang::nif::atom(env, "source"),
            enif_make_int(env, (int)buf[i].event_source), &ev);
        events = enif_make_list_cell(env, ev, events);
    }
    return erlang::nif::ok(env, events);
}

// litert_compiled_model_reset_profile(Model) -> ok | {error, Reason}
ERL_NIF_TERM litert_compiled_model_reset_profile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);
    CompiledModelUse res(env, argv[0]);
    if (!res) return res.error();
    if (res->profiler == nullptr) {
        return erlang::nif::error(env, "this model was not compiled with profiling on");
    }
    // ProfileBuffer::Reset clears enabled_, and LiteRT's outer reset does not put
    // it back, so a reset without this leaves a model that silently records
    // nothing for the rest of its life.
    LiteRtStatus st = LiteRtResetProfiler(res->profiler);
    if (st != kLiteRtStatusOk) return litert_error(env, "reset profiler", st);
    st = LiteRtStartProfiler(res->profiler);
    if (st != kLiteRtStatusOk) return litert_error(env, "restart profiler after reset", st);
    return erlang::nif::ok(env);
}

// litert_compiled_model_io_sizes(Model) -> {ok, {[integer()], [integer()]}}
ERL_NIF_TERM litert_compiled_model_io_sizes(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);
    CompiledModelUse res(env, argv[0]);
    if (!res) return res.error();

    auto to_list = [&](const std::vector<size_t> &v) {
        ERL_NIF_TERM list = enif_make_list(env, 0);
        for (size_t i = v.size(); i-- > 0; ) {
            list = enif_make_list_cell(env, enif_make_uint64(env, v[i]), list);
        }
        return list;
    };
    return erlang::nif::ok(env, enif_make_tuple2(env,
        to_list(*res->input_sizes), to_list(*res->output_sizes)));
}

// litert_model_signatures(Env, Path) -> {ok, [binary()]} | {error, Reason}
//
// The keys, in index order, so a caller can name a signature rather than count
// to it. Reading them needs a model but not a compile, which is why this takes
// a path rather than a compiled model.
ERL_NIF_TERM litert_model_signatures(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM error{};
    auto env_res = NifResLiteRtEnvironment::get_resource(env, argv[0], error);
    if (env_res == nullptr) return error;

    std::string path;
    if (!erlang::nif::get(env, argv[1], path)) {
        return erlang::nif::error(env, "expecting the model path to be a string");
    }

    LiteRtModel model = nullptr;
    LiteRtStatus st = LiteRtCreateModelFromFile(env_res->val, path.c_str(), &model);
    if (st != kLiteRtStatusOk) return litert_error(env, "load model", st);

    LiteRtParamIndex count = 0;
    st = LiteRtGetNumModelSignatures(model, &count);
    if (st != kLiteRtStatusOk) { LiteRtDestroyModel(model); return litert_error(env, "signature count", st); }

    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (LiteRtParamIndex i = count; i-- > 0; ) {
        LiteRtSignature sig = nullptr;
        const char *key = nullptr;
        if (LiteRtGetModelSignature(model, i, &sig) != kLiteRtStatusOk ||
            LiteRtGetSignatureKey(sig, &key) != kLiteRtStatusOk) {
            LiteRtDestroyModel(model);
            return erlang::nif::error(env, "signature key");
        }
        list = enif_make_list_cell(env, erlang::nif::make_binary(env, key ? key : ""), list);
    }
    LiteRtDestroyModel(model);
    return erlang::nif::ok(env, list);
}

// litert_compiled_model_metrics(Model, DetailLevel) -> {ok, [{binary(), term()}]}
//
// Hardware counters, which an accelerator supplies through the two entries of
// its definition that may be null. Neither the plugins here nor Google's own
// prebuilt fills them in, so this is usually an empty list rather than an
// error, and saying so beats leaving a caller to wonder.
ERL_NIF_TERM litert_compiled_model_metrics(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    CompiledModelUse res(env, argv[0]);
    if (!res) return res.error();

    int detail = 0;
    if (!enif_get_int(env, argv[1], &detail) || detail < 0) {
        return erlang::nif::error(env, "expecting a detail level of zero or more");
    }

    // The metrics object first: creating it allocates, and an allocation that
    // throws between starting and stopping collection would leave the backend
    // collecting for ever.
    LiteRtMetrics metrics = nullptr;
    LiteRtStatus st = LiteRtCreateMetrics(&metrics);
    if (st != kLiteRtStatusOk) return litert_error(env, "create metrics", st);

    st = LiteRtCompiledModelStartMetricsCollection(res->val, detail);
    if (st != kLiteRtStatusOk) { LiteRtDestroyMetrics(metrics); return litert_error(env, "start metrics", st); }

    st = LiteRtCompiledModelStopMetricsCollection(res->val, metrics);
    if (st != kLiteRtStatusOk) { LiteRtDestroyMetrics(metrics); return litert_error(env, "stop metrics", st); }

    int n = 0;
    if (LiteRtGetNumMetrics(metrics, &n) != kLiteRtStatusOk) {
        LiteRtDestroyMetrics(metrics);
        return erlang::nif::error(env, "metric count");
    }

    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (int i = n; i-- > 0; ) {
        LiteRtMetric metric{};
        if (LiteRtGetMetric(metrics, i, &metric) != kLiteRtStatusOk) continue;
        ERL_NIF_TERM value;
        switch (metric.value.type) {
            case kLiteRtAnyTypeInt:    value = enif_make_int64(env, metric.value.int_value); break;
            case kLiteRtAnyTypeReal:   value = enif_make_double(env, metric.value.real_value); break;
            case kLiteRtAnyTypeBool:   value = erlang::nif::atom(env, metric.value.bool_value ? "true" : "false"); break;
            case kLiteRtAnyTypeString: value = erlang::nif::make_binary(env, metric.value.str_value ? metric.value.str_value : ""); break;
            default:                   value = erlang::nif::atom(env, "unsupported"); break;
        }
        list = enif_make_list_cell(env, enif_make_tuple2(env,
            erlang::nif::make_binary(env, metric.name ? metric.name : ""), value), list);
    }
    LiteRtDestroyMetrics(metrics);
    return erlang::nif::ok(env, list);
}

// litert_platform_support() -> map()
//
// What this build of the library can reach, not what the machine has. These are
// compile-time answers, from the macros in litert_common.h and from what this
// build turns off: OpenCL reads false everywhere because CMakeLists defines
// LITERT_DISABLE_OPENCL_SUPPORT. Whether a device is actually there is
// discovered by asking for it and being refused.
ERL_NIF_TERM litert_platform_support(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);

    ERL_NIF_TERM map = enif_make_new_map(env);
    auto put = [&](const char *name, bool value) {
        enif_make_map_put(env, map, erlang::nif::atom(env, name),
                          erlang::nif::atom(env, value ? "true" : "false"), &map);
    };
    put("opencl",     LiteRtHasOpenClSupport());
    put("opengl",     LiteRtHasOpenGlSupport());
    put("metal",      LiteRtHasMetalSupport());
    put("ahwb",       LiteRtHasAhwbSupport());
    put("ion",        LiteRtHasIonSupport());
    put("dmabuf",     LiteRtHasDmaBufSupport());
    put("fastrpc",    LiteRtHasFastRpcSupport());
    put("sync_fence", LiteRtHasSyncFenceSupport());
    return map;
}

// litert_compiled_model_controlling_process(Model, Pid) -> ok | {error, Reason}
//
// Binds the model to one process. Deliberately not done by new/6: the direct
// API is meant to be usable from wherever a caller holds the reference, and it
// is the server that wants the model to itself.
ERL_NIF_TERM litert_compiled_model_set_controlling_process(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    CompiledModelUse res(env, argv[0]);
    if (!res) return res.error();

    ErlNifPid pid;
    if (!enif_get_local_pid(env, argv[1], &pid)) {
        return erlang::nif::error(env, "expecting a local pid");
    }
    if (!enif_is_process_alive(env, &pid)) {
        return erlang::nif::error(env, "that process is not alive");
    }

    res->controlling_process = pid;
    res->is_controlled = true;
    return erlang::nif::ok(env);
}

// litert_compiled_model_controlling_process(Model) -> {ok, pid()} | undefined
//
// Reads the claim without being subject to it, the same way
// interpreter_controlling_process does: a process that has just handed a model
// away still has a reason to ask who has it.
ERL_NIF_TERM litert_compiled_model_get_controlling_process(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResLiteRtCompiledModel * res = nullptr;
    if (!enif_get_resource(env, argv[0], NifResLiteRtCompiledModel::type, (void **)&res) || res == nullptr) {
        return erlang::nif::error(env, "cannot access NifResLiteRtCompiledModel resource");
    }
    if (!res->is_controlled) return erlang::nif::atom(env, "undefined");
    return erlang::nif::ok(env, enif_make_pid(env, &res->controlling_process));
}

#endif  // TFLITE_BEAM_LITERT_API_ENABLED
