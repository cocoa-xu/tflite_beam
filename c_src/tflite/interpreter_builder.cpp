#include <string>
#include <vector>

#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"
#include "../helper.h"

#include "tensorflow/lite/model_builder.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/interpreter_builder.h"
#include "tensorflow/lite/core/interpreter.h"

#include "interpreter_builder.h"
#include "delegate.h"
#include "status.h"

ERL_NIF_TERM interpreter_builder_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM model_nif = argv[0];
    ERL_NIF_TERM resolver_nif = argv[1];
    NifResFlatBufferModel * model_res = nullptr;
    NifResBuiltinOpResolver * resolver_res = nullptr;
    NifResInterpreterBuilder * res = nullptr;
    ERL_NIF_TERM ret;

    if (!enif_get_resource(env, model_nif, NifResFlatBufferModel::type, (void **)&model_res) || model_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResFlatBufferModel resource");
    }

    if (!enif_get_resource(env, resolver_nif, NifResBuiltinOpResolver::type, (void **)&resolver_res) || resolver_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResBuiltinOpResolver resource");
    }

    if (!(res = NifResInterpreterBuilder::allocate_resource(env, ret))) {
        return ret;
    }

    res->val = new tflite::InterpreterBuilder(*model_res->val, *resolver_res->val);

    // the builder outlives the terms these came in as, so hold real references
    res->op_resolver = resolver_res;
    enif_keep_resource(resolver_res);

    res->flatbuffer_model = model_res;
    enif_keep_resource(model_res);
    ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return erlang::nif::ok(env, ret);
}

ERL_NIF_TERM interpreter_builder_add_delegate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    NifResInterpreterBuilder * self_res;
    NifResDelegate * delegate_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreterBuilder::get_resource(env, argv[0], ret))) {
        return ret;
    }

    if (!(delegate_res = NifResDelegate::get_resource(env, argv[1], ret))) {
        return ret;
    }

    std::string on_decline;
    if (!erlang::nif::get_atom(env, argv[2], on_decline) ||
        (on_decline != "error" && on_decline != "fallback")) {
        return erlang::nif::error(env, "expecting on_decline to be either error or fallback");
    }

    // AddDelegate takes no ownership, and the delegate has to outlive every
    // interpreter this builder goes on to produce, so the reference is ours to
    // hold until the builder itself is collected
    self_res->val->AddDelegate(delegate_res->val);
    enif_keep_resource(delegate_res);
    self_res->delegates->push_back({delegate_res, on_decline == "fallback"});

    return erlang::nif::ok(env);
}

// Facts, so that the decision about a default delegate can be taken in Erlang
// where it is visible, rather than here where it would not be.
ERL_NIF_TERM interpreter_builder_state(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResInterpreterBuilder * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreterBuilder::get_resource(env, argv[0], ret))) {
        return ret;
    }

    const bool applies_default_delegates =
        self_res->op_resolver == nullptr || self_res->op_resolver->apply_default_delegates;

    return erlang::nif::ok(env, enif_make_tuple3(
        env,
        enif_make_uint64(env, (ErlNifUInt64)self_res->delegates->size()),
        enif_make_int(env, self_res->num_threads),
        applies_default_delegates ? erlang::nif::atom(env, "true") : erlang::nif::atom(env, "false")
    ));
}

static bool any_delegate_steps_aside(NifResInterpreterBuilder * self_res) {
    for (auto & entry : *self_res->delegates) {
        if (entry.fallback_on_decline) return true;
    }
    return false;
}

// A delegate that cannot take the graph, but leaves it runnable, reports
// kTfLiteApplicationError rather than kTfLiteError -- and operator() discards
// the interpreter either way. Building again without the delegates that said
// they would step aside is what turns that back into a working CPU interpreter.
//
// It needs a builder of its own: AddDelegate only appends and the list it
// appends to is private. The model, the op resolver and the thread count are
// the whole of the original builder's state that this binding ever sets.
static TfLiteStatus build_without_delegates_that_step_aside(
        NifResInterpreterBuilder * self_res,
        std::unique_ptr<tflite::Interpreter> * interpreter,
        std::vector<NifResDelegate *> & applied) {
    tflite::InterpreterBuilder retry(*self_res->flatbuffer_model->val, *self_res->op_resolver->val);
    if (self_res->num_threads != -1) {
        retry.SetNumThreads(self_res->num_threads);
    }

    applied.clear();
    for (auto & entry : *self_res->delegates) {
        if (!entry.fallback_on_decline) {
            retry.AddDelegate(entry.delegate->val);
            applied.push_back(entry.delegate);
        }
    }

    return retry(interpreter);
}

ERL_NIF_TERM interpreter_builder_build(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM interpreter_nif = argv[1];
    NifResInterpreterBuilder * self_res;
    NifResInterpreter * interpreter_res;

    if (!enif_get_resource(env, self_nif, NifResInterpreterBuilder::type, (void **)&self_res) || self_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResInterpreterBuilder resource");
    }

    if (!enif_get_resource(env, interpreter_nif, NifResInterpreter::type, (void **)&interpreter_res) || interpreter_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResInterpreter resource");
    }

    // operator() destroys the interpreter these were taken from, whether it goes on
    // to succeed or not, so the cache cannot outlive this call. Once, before the
    // first attempt: the retry below resets an interpreter that is already gone.
    NifResInterpreter::release_tensors(interpreter_res);
    NifResInterpreter::release_signature_runners(interpreter_res);

    std::vector<NifResDelegate *> applied;
    for (auto & entry : *self_res->delegates) {
        applied.push_back(entry.delegate);
    }

    std::unique_ptr<tflite::Interpreter> pretend(interpreter_res->val);
    TfLiteStatus status = self_res->val->operator()(&pretend);

    // the retry builds from the model and resolver this builder was made with, so
    // it is only on the table while both are still there
    const bool can_rebuild = self_res->flatbuffer_model && self_res->flatbuffer_model->val &&
                             self_res->op_resolver && self_res->op_resolver->val;

    bool declined = false;
    if (status == kTfLiteApplicationError && can_rebuild && any_delegate_steps_aside(self_res)) {
        status = build_without_delegates_that_step_aside(self_res, &pretend, applied);
        declined = (status == kTfLiteOk);
    }

    interpreter_res->val = pretend.release();

    if (interpreter_res->val == nullptr) {
        applied.clear();
    }

    // keep before release, so a delegate on both lists cannot transiently reach
    // refcount zero. Safe to release the old ones here and not earlier: the
    // interpreter they backed was destroyed inside operator().
    if (interpreter_res->delegates) {
        for (auto delegate_res : applied) {
            enif_keep_resource(delegate_res);
        }
        for (auto delegate_res : *interpreter_res->delegates) {
            enif_release_resource(delegate_res);
        }
        *interpreter_res->delegates = applied;
    }

    if (interpreter_res->flatbuffer_model) {
        enif_release_resource(interpreter_res->flatbuffer_model);
    }
    interpreter_res->flatbuffer_model = self_res->flatbuffer_model;
    enif_keep_resource(interpreter_res->flatbuffer_model);

    if (declined) {
        return erlang::nif::ok(env, erlang::nif::atom(env, "delegate_declined"));
    }
    return tflite_status_to_erl_term(env, status);
}

ERL_NIF_TERM interpreter_builder_set_num_threads(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM num_threads_nif = argv[1];
    int num_threads = -1;
    NifResInterpreterBuilder * self_res;

    if (!enif_get_resource(env, self_nif, NifResInterpreterBuilder::type, (void **)&self_res) || self_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResInterpreterBuilder resource");
    }

    if (!erlang::nif::get(env, num_threads_nif, &num_threads)) {
        return erlang::nif::error(env, "expecting num_threads to be an integer");
    }

    auto status = self_res->val->SetNumThreads(num_threads);
    if (status == kTfLiteOk) {
        self_res->num_threads = num_threads;
    }
    return tflite_status_to_erl_term(env, status);
}
