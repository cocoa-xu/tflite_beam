#ifndef TFLITE_BEAM_ERLANG_NIF_RESOURCE_H
#define TFLITE_BEAM_ERLANG_NIF_RESOURCE_H

#pragma once

#include <atomic>
#include <new>
#include <memory>
#include <string>
#include <vector>
#include <erl_nif.h>
#include "tsan_annotations.h"

#include "tflite/c/c_api.h"
#include "tflite/c/common.h"
#include "tflite/core/api/error_reporter.h"
#include "tflite/interpreter_builder.h"
#include "tflite/interpreter.h"
#include "tflite/signature_runner.h"
#include "tflite/kernels/register.h"
#include "tflite/kernels/builtin_op_kernels.h"
#include "tflite/model.h"
#include "tflite/stderr_reporter.h"

#ifdef TFLITE_BEAM_LITERT_API_ENABLED
#include "litert/c/litert_common.h"
#include "litert/c/litert_compiled_model.h"
#include "litert/c/litert_environment.h"
#include "litert/c/litert_model.h"
#include "litert/c/litert_options.h"
#include "litert/c/litert_profiler.h"
#include "litert/c/litert_tensor_buffer.h"
#endif
// enif_alloc_resource returns raw storage and runs no constructor, so a
// std::atomic in a resource is an object that never began: assigning to it is
// undefined behaviour rather than a store. These two put it in place and take it
// out again, and every resource holding one has to call both.
template <typename T>
inline void construct_atomic(std::atomic<T> * where, T value) {
    new (static_cast<void *>(where)) std::atomic<T>(value);
}

template <typename T>
inline void destroy_atomic(std::atomic<T> * what) {
    what->~atomic();
}

// enif_alloc_resource hands back a resource carrying one reference, and that
// reference belongs to nobody until enif_make_resource passes it to a term.
// Everything in between is a chance to lose it: the resource is live, no term
// names it, and if the function returns another way its destructor will never
// run. Holding it here means the only way out that keeps it is the one that
// hands it to a term.
template <typename T>
class ResourceRef {
public:
    explicit ResourceRef(T * res) : res_(res) {}
    // The release is where the resource stops being this thread's, whether it
    // is going to Erlang or straight to its destructor, so it is also the point
    // everything written into it becomes someone else's to read.
    ~ResourceRef() {
        if (res_) {
            TFLITE_BEAM_TSAN_PUBLISH(res_);
            enif_release_resource(res_);
        }
    }

    ResourceRef(const ResourceRef &) = delete;
    ResourceRef & operator=(const ResourceRef &) = delete;

    T * get() const { return res_; }
    T * operator->() const { return res_; }
    explicit operator bool() const { return res_ != nullptr; }


private:
    T * res_;
};

// A lock that survives the way out. enif_mutex_unlock on the line after the one
// that allocates is not reached when the allocation throws, and a mutex nobody
// unlocks is worse than the leak that got it there: every later reader of the
// same structure waits forever.
// Same shape as MutexLock, but reports the collision rather than waiting for it,
// which is what every lock on an interpreter wants: a NIF that blocks is a
// scheduler that blocks.
class MutexTryLock {
public:
    explicit MutexTryLock(ErlNifMutex * mutex) : mutex_(mutex) {
        acquired_ = mutex_ != nullptr && enif_mutex_trylock(mutex_) == 0;
    }
    ~MutexTryLock() { if (acquired_) enif_mutex_unlock(mutex_); }

    MutexTryLock(const MutexTryLock &) = delete;
    MutexTryLock & operator=(const MutexTryLock &) = delete;

    bool acquired() const { return acquired_; }

private:
    ErlNifMutex * mutex_;
    bool acquired_;
};

class MutexLock {
public:
    explicit MutexLock(ErlNifMutex * mutex) : mutex_(mutex) {
        if (mutex_) enif_mutex_lock(mutex_);
    }
    ~MutexLock() { if (mutex_) enif_mutex_unlock(mutex_); }

    MutexLock(const MutexLock &) = delete;
    MutexLock & operator=(const MutexLock &) = delete;

private:
    ErlNifMutex * mutex_;
};

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

    static ErlNifResourceType * type;
    static NifResErrorReporter * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResErrorReporter * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResFlatBufferModel {
    tflite::FlatBufferModel * val;

    // copy the buffer when build from buffer
    const char * copied_buffer;

    // TFLite keeps the reporter pointer for the model's lifetime, so the
    // resource holding it is kept alive here with enif_keep_resource. nullptr
    // when the model was built with the default reporter, which is a static
    // singleton and outlives everything.
    NifResErrorReporter * error_reporter;

    static ErlNifResourceType * type;
    static NifResFlatBufferModel * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResFlatBufferModel * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

#ifdef TFLITE_BEAM_LITERT_API_ENABLED

// A LiteRT environment. It is what a GPU accelerator plugin is found through,
// so it carries the directory that plugin was looked for in and nothing else.
struct NifResLiteRtEnvironment {
    LiteRtEnvironment val;

    static ErlNifResourceType * type;
    static NifResLiteRtEnvironment * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResLiteRtEnvironment * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

// A compiled model together with everything whose lifetime it depends on. The
// model and the options outlive the compile in LiteRT's C API, and the input
// and output buffers belong to the compiled model rather than to a call, which
// is the whole reason a caller has to serialise access to one of these.
struct NifResLiteRtCompiledModel {
    LiteRtCompiledModel val;
    LiteRtModel model;
    LiteRtOptions options;
    // kept alive because the compiled model holds a plain pointer to it
    NifResLiteRtEnvironment * environment;

    std::vector<LiteRtTensorBuffer> * inputs;
    std::vector<LiteRtTensorBuffer> * outputs;
    std::vector<size_t> * input_sizes;
    std::vector<size_t> * output_sizes;

    // the buffers above belong to one signature, so the model remembers which
    LiteRtParamIndex signature;

    // LiteRT says the compiled model API is not verified for multithreading
    // (litert_compiled_model.cc) and the profile buffer under it says outright
    // that it is not thread safe (tflite/profiling/profile_buffer.h). Two
    // schedulers entering any of these NIFs on one resource is therefore a data
    // race, not merely crossed outputs, so every operation takes this lock and
    // holds it across the ownership check as well. enif_alloc_resource hands
    // back raw storage and runs no constructor, hence a mutex handle rather
    // than a std::mutex, and hence plain fields under this lock rather than a
    // std::atomic that nothing would have constructed.
    ErlNifMutex * lock;
    bool is_controlled;
    ErlNifPid controlling_process;

    // borrowed from the compiled model, not owned
    LiteRtProfiler profiler;

    static ErlNifResourceType * type;
    static NifResLiteRtCompiledModel * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResLiteRtCompiledModel * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

#endif  // TFLITE_BEAM_LITERT_API_ENABLED

#ifdef TFLITE_BEAM_LITERT_API_ENABLED
// Declared here so the compiled model NIFs can hold the resource lock across
// both the ownership check and the operation it guards.
bool compiled_model_caller_may_use(ErlNifEnv * env, NifResLiteRtCompiledModel * res);
#endif

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
    // Both registries are non-owning, and both exist for one reason: to reach
    // the handles that borrow from this interpreter when what they borrow stops
    // being theirs. A tensor holds a TfLiteTensor * out of the arena, a runner
    // holds a SignatureRunner *, and neither pointer survives an AllocateTensors,
    // a reshape, or a rebuild.
    //
    // Non-owning both ways round, because the borrower is what keeps this
    // interpreter alive, not the other way about. Taking a reference here as
    // well would close the loop and neither end would ever be collected.
    std::vector<NifResTfLiteTensor *> * tensors;
    std::vector<NifResSignatureRunner *> * signature_runners;
    // One lock each, guarding the registry alone and not the interpreter.
    // Insertion happens on a normal scheduler, removal in a destructor that can
    // run on any thread, and the clear on a dirty one, so they need their own
    // locks even where the caller already holds in_use.
    ErlNifMutex * tensors_lock;
    ErlNifMutex * signature_runners_lock;
    // the delegates behind this interpreter's graph, kept alive for its lifetime
    std::vector<NifResDelegate *> * delegates;
    // held while a NIF is inside this interpreter, so a second thread arriving
    // during that window can be told rather than left to race. A pointer, like
    // every other non-POD member here: enif_alloc_resource hands back raw memory
    // and runs no constructor on it.
    ErlNifMutex * in_use;
    // Held while val itself is being replaced, which only the builder does.
    // Cancel cannot take in_use, since the whole point of it is to be called
    // from another process while invoke holds that one, but it does dereference
    // val and the builder deletes val. So the two meet here instead, and invoke
    // is not in the way.
    ErlNifMutex * being_replaced;
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
    // for calls that may move tensors rather than existing to move them:
    // retire only the handles whose index now resolves somewhere else
    static void revalidate_tensors(NifResInterpreter * res);
};

// tflite::Interpreter is documented as not thread-safe, and invoke runs on a
// dirty scheduler, so two processes sharing one interpreter really do reach it
// on two OS threads. This makes that arrival visible instead of silent: whoever
// gets there second is told, rather than quietly reading half of someone else's
// inference.
// Declared here so signature runners can ask the same question about the
// interpreter they belong to.
bool caller_may_use(ErlNifEnv * env, NifResInterpreter * res);


// Every entry point that dereferences an interpreter takes the guard, not only
// the ones that write, and re-reads val after taking it. get_resource checks val
// before the guard, which proves only that it was not null a moment ago: a
// rebuild that fails stores null, and a caller that passed the earlier check and
// was then descheduled would go on to dereference it. The check that counts is
// the one under the lock.
// interpreter_builder_build deletes the tflite::Interpreter
// and puts another in its place, so a reader holding nothing but self_res is
// holding a pointer to something that can be freed while it reads. There is one
// exception, and it is the reason this is a macro rather than a helper that
// returns a value: cancel exists to be called from another process while invoke
// holds the guard, so it must not take it.
// For cancel, and only cancel. See being_replaced above.
#define TFLITE_BEAM_INTERPRETER_NOT_BEING_REPLACED(RES)                              \
    MutexTryLock not_replacing((RES)->being_replaced);                               \
    if (!not_replacing.acquired()) {                                                 \
        return erlang::nif::error(env, "interpreter is being rebuilt");               \
    }

// The ownership check happens here rather than only in get_resource, and it
// happens under this guard. Checking before taking the guard leaves a window: a
// caller can pass the check on an unclaimed interpreter, be descheduled, watch
// somebody else claim it, and then acquire the guard and use an interpreter it
// no longer has any authority over. Checking again inside closes that, and the
// guard is also what makes the plain pid safe to read at all.
#define TFLITE_BEAM_INTERPRETER_IN_USE(RES)                                          \
    MutexTryLock in_use((RES)->in_use);                                              \
    if (!in_use.acquired()) {                                                        \
        return erlang::nif::error(env,                                               \
            "interpreter is already in use by another process");                     \
    }                                                                                \
    if (!caller_may_use(env, (RES))) {                                               \
        return erlang::nif::error(env, "interpreter belongs to another process");    \
    }                                                                                \
    if ((RES)->val == nullptr) {                                                     \
        return erlang::nif::error(env, "cannot access NifResInterpreter resource");  \
    }

// The same for a handle that borrows from an interpreter, plus the part that
// makes the borrow safe: the dead flag is read once by get_resource and once
// here. A rebuild already running when the first read happened finishes while
// this waits for the guard, and the second read is what catches it. Without it
// the first read only proves the handle was alive a moment ago.
#define TFLITE_BEAM_BORROWED_IN_USE(RES, RETIRED_MESSAGE)                            \
    MutexTryLock in_use((RES)->interpreter == nullptr                                \
                            ? nullptr : (RES)->interpreter->in_use);                  \
    if ((RES)->interpreter != nullptr && !in_use.acquired()) {                       \
        return erlang::nif::error(env,                                               \
            "interpreter is already in use by another process");                     \
    }                                                                                \
    if ((RES)->interpreter != nullptr && !caller_may_use(env, (RES)->interpreter)) { \
        return erlang::nif::error(env, "interpreter belongs to another process");    \
    }                                                                                \
    if ((RES)->interpreter_has_gone || (RES)->val == nullptr ||                      \
        ((RES)->interpreter != nullptr && (RES)->interpreter->val == nullptr)) {     \
        return erlang::nif::error(env, RETIRED_MESSAGE);                             \
    }

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
    // Borrowed, always. The interpreter owns every TfLiteTensor this can point
    // at, and interpreter_tensor is the only place one of these is made.
    TfLiteTensor * val;
    // kept alive with enif_keep_resource. val points into the interpreter's
    // arena, so a handle that outlives its interpreter is a handle into freed
    // memory, and Erlang gives no warning before that happens: the compiler
    // stops counting a variable as live at its last mention, so an interpreter
    // fetched from, then never named again, is collectable while the tensor
    // taken out of it is still in use.
    NifResInterpreter * interpreter;
    // which tensor this is, so that after a call that may have moved them the
    // handle can be asked whether its own pointer is still the right one
    int index;
    // and keeping the interpreter alive is not enough on its own, because
    // AllocateTensors and a reshape both move the arena out from under val
    // while the interpreter itself stays exactly where it was
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
