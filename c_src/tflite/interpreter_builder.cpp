#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"
#include "../helper.h"

#include "tensorflow/lite/model_builder.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/interpreter_builder.h"
#include "tensorflow/lite/core/interpreter.h"

#include "interpreter_builder.h"
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
    res->op_resolver = resolver_res;
    resolver_res->reference_count++;

    res->flatbuffer_model = model_res;
    res->flatbuffer_model->reference_count++;
    ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return erlang::nif::ok(env, ret);
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

    std::unique_ptr<tflite::Interpreter> pretend(interpreter_res->val);
    self_res->val->operator()(&pretend);

    interpreter_res->val = pretend.release();
    interpreter_res->flatbuffer_model = self_res->flatbuffer_model;
    interpreter_res->flatbuffer_model->reference_count++;
    return erlang::nif::ok(env);
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
    return tflite_status_to_erl_term(env, status);
}
