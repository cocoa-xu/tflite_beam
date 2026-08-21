#ifndef TFLITE_BEAM_ERLANG_NIF_RESOURCE_H
#define TFLITE_BEAM_ERLANG_NIF_RESOURCE_H

#pragma once

#include <atomic>
#include <memory>
#include <string>
#include <vector>
#include <erl_nif.h>

#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"
#include "tensorflow/lite/core/api/error_reporter.h"
#include "tensorflow/lite/interpreter_builder.h"
#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/signature_runner.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/kernels/builtin_op_kernels.h"
#include "tensorflow/lite/model.h"
#include "tensorflow/lite/stderr_reporter.h"

struct NifResBuiltinOpResolver {
    tflite::ops::builtin::BuiltinOpResolver * val;
    // whether this resolver hands the interpreter TfLite's own lazily-applied
    // delegates. The builder reads it to decide whether to attach one itself.
    bool apply_default_delegates;

    static ErlNifResourceType * type;
    static NifResBuiltinOpResolver * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResBuiltinOpResolver * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResErrorReporter {
    tflite::ErrorReporter * val;
    std::atomic_bool is_default;

    static ErlNifResourceType * type;
    static NifResErrorReporter * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResErrorReporter * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResFlatBufferModel {
    tflite::FlatBufferModel * val;

    // copy the buffer when build from buffer
    const char * copied_buffer;

    static ErlNifResourceType * type;
    static NifResFlatBufferModel * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResFlatBufferModel * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResDelegate {
    TfLiteDelegate * val;
    // the C factory's matching destructor; these pointers never go to `delete`
    void (*deleter)(TfLiteDelegate *);
    // a factory may keep the string pointer it was handed rather than copying
    // it, so the resource owns any such buffer for as long as the delegate lives
    char * owned_path;

    static ErlNifResourceType * type;
    static NifResDelegate * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResDelegate * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

// a delegate attached to a builder, with what to do if it declines the graph
struct NifResDelegateEntry {
    NifResDelegate * delegate;
    bool fallback_on_decline;
};

struct NifResInterpreterBuilder {
    tflite::InterpreterBuilder * val;
    // kept alive with enif_keep_resource for as long as this builder holds them
    NifResBuiltinOpResolver * op_resolver;
    NifResFlatBufferModel * flatbuffer_model;
    // AddDelegate takes no ownership, so every delegate here is kept alive by us
    // until the builder goes. Heap-allocated: enif_alloc_resource runs no
    // constructor, so a by-value vector member would be undefined behaviour
    std::vector<NifResDelegateEntry> * delegates;
    // remembered so a decline can be retried on a builder without the delegates
    int num_threads;

    static ErlNifResourceType * type;
    static NifResInterpreterBuilder * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResInterpreterBuilder * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResTfLiteTensor;
struct NifResSignatureRunner;
struct NifResEdgeTpuContext;
struct NifResInterpreter {
    tflite::Interpreter * val;
    // kept alive with enif_keep_resource; the interpreter reads the model's buffer
    NifResFlatBufferModel * flatbuffer_model;
    // kept alive the same way; an Edge TPU interpreter delegates to this context.
    // nullptr for interpreters that do not run on a TPU
    NifResEdgeTpuContext * edgetpu_context;
    // Owning. interpreter_tensor hands the reference from allocate_resource to
    // this map, and release_tensors gives it back. A handle therefore lives as
    // long as the cache does, whatever Erlang is still holding.
    std::map<int, NifResTfLiteTensor *> * tensors;
    // Non-owning, which is the opposite of the map above and deliberately so.
    // Runners are created and handed straight to Erlang, so taking a reference
    // here would keep every runner ever asked for alive until the interpreter
    // went away. Each removes itself in its destructor. Both containers exist
    // for the same reason, to reach the borrowers when the interpreter is
    // replaced, and they get there by opposite routes.
    std::vector<NifResSignatureRunner *> * signature_runners;
    // Guards signature_runners alone, not the interpreter. Insertion happens on a
    // normal scheduler, removal in a destructor that can run on any thread, and
    // the clear on a dirty one, so the vector needs its own lock even where the
    // caller already holds in_use.
    ErlNifMutex * signature_runners_lock;
    // the delegates behind this interpreter's graph, kept alive for its lifetime
    std::vector<NifResDelegate *> * delegates;
    // held while a NIF is inside this interpreter, so a second thread arriving
    // during that window can be told rather than left to race. A pointer, like
    // every other non-POD member here: enif_alloc_resource hands back raw memory
    // and runs no constructor on it.
    ErlNifMutex * in_use;
    // opt-in: once a process takes control, everyone else is refused. Unset by
    // default, because an interpreter is born shared and making it otherwise
    // would break every caller that builds in one process and runs in another.
    std::atomic_bool is_controlled;
    ErlNifPid controlling_process;

    static ErlNifResourceType * type;
    static NifResInterpreter * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResInterpreter * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
    // every cached tensor borrows a TfLiteTensor * from the interpreter, so they all
    // have to go whenever that interpreter does
    static void release_tensors(NifResInterpreter * res);
    // and so does every signature runner, which borrows a SignatureRunner *
    static void release_signature_runners(NifResInterpreter * res);
};

// tflite::Interpreter is documented as not thread-safe, and invoke runs on a
// dirty scheduler, so two processes sharing one interpreter really do reach it
// on two OS threads. This makes that arrival visible instead of silent: whoever
// gets there second is told, rather than quietly reading half of someone else's
// inference.
// Declared here so signature runners can ask the same question about the
// interpreter they belong to.
bool caller_may_use(ErlNifEnv * env, NifResInterpreter * res);

class InterpreterInUse {
public:
    explicit InterpreterInUse(NifResInterpreter * res) : res_(res) {
        // try, never wait: the point is to report the collision, not to
        // serialise around it on a dirty scheduler
        acquired_ = res_->in_use != nullptr && enif_mutex_trylock(res_->in_use) == 0;
    }

    ~InterpreterInUse() {
        if (acquired_) enif_mutex_unlock(res_->in_use);
    }

    InterpreterInUse(const InterpreterInUse &) = delete;
    InterpreterInUse & operator=(const InterpreterInUse &) = delete;

    bool acquired() const { return acquired_; }

private:
    NifResInterpreter * res_;
    bool acquired_;
};

struct NifResSignatureRunner {
    // owned by the interpreter that handed it out, so never deleted here
    tflite::SignatureRunner * val;
    // kept alive with enif_keep_resource: the runner dies with its interpreter
    NifResInterpreter * interpreter;
    // keeping the interpreter alive is not enough: building into it again
    // destroys the tflite::Interpreter this borrows from and puts a new one in
    // its place, which leaves val dangling with the resource still valid
    std::atomic_bool interpreter_has_gone;

    static ErlNifResourceType * type;
    static NifResSignatureRunner * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResSignatureRunner * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResTfLiteTensor {
    TfLiteTensor * val;
    std::atomic_bool borrowed;
    std::atomic_bool interpreter_has_gone;

    static ErlNifResourceType * type;
    static NifResTfLiteTensor * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResTfLiteTensor * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

#ifdef CORAL_SUPPORT_ENABLED

#include "tflite/public/edgetpu.h"

struct NifResEdgeTpuContext {
    edgetpu::EdgeTpuContext * val;
    // owns the context: the device is handed back once every resource sharing it is gone
    std::shared_ptr<edgetpu::EdgeTpuContext> * context;

    static ErlNifResourceType * type;
    static NifResEdgeTpuContext * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResEdgeTpuContext * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

#endif

#endif //TFLITE_BEAM_ERLANG_NIF_RESOURCE_HPP
