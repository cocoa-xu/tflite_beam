#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.hpp"
#include "../helper.h"

#include "tensorflow/lite/model_builder.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/interpreter_builder.h"
#include "tensorflow/lite/core/interpreter.h"

#include "interpreter_builder.h"
#include "status.h"

ERL_NIF_TERM interpreterBuilder_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM model_nif = argv[0];
    ERL_NIF_TERM resolver_nif = argv[1];
    NifResFlatBufferModel * model_res = nullptr;
    NifResBuiltinOpResolver * resolver_res = nullptr;
    NifResInterpreterBuilder * res = nullptr;

    if (enif_get_resource(env, model_nif, NifResFlatBufferModel::type, (void **)&model_res) &&
        enif_get_resource(env, resolver_nif, NifResBuiltinOpResolver::type, (void **)&resolver_res) &&
        (res = alloc_resource_NifResInterpreterBuilder())) {
        if (model_res->val && resolver_res->val) {
            res->val = new tflite::InterpreterBuilder(*model_res->val, *resolver_res->val);
            res->op_resolver = resolver_res;
            resolver_res->reference_count++;

            res->flatbuffer_model = model_res;
            res->flatbuffer_model->reference_count++;
            ERL_NIF_TERM ret = enif_make_resource(env, res);
            enif_release_resource(res);
            return erlang::nif::ok(env, ret);
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

ERL_NIF_TERM interpreterBuilder_build(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM interpreter_nif = argv[1];
    NifResInterpreterBuilder * self_res;
    NifResInterpreter * interpreter_res;
    if (enif_get_resource(env, self_nif, NifResInterpreterBuilder::type, (void **)&self_res) &&
        enif_get_resource(env, interpreter_nif, NifResInterpreter::type, (void **)&interpreter_res)) {
        if (self_res->val && interpreter_res->val) {
            std::unique_ptr<tflite::Interpreter> pretend(interpreter_res->val);
            self_res->val->operator()(&pretend);

            interpreter_res->val = pretend.release();
            interpreter_res->flatbuffer_model = self_res->flatbuffer_model;
            interpreter_res->flatbuffer_model->reference_count++;
            return erlang::nif::ok(env);
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

ERL_NIF_TERM interpreterBuilder_setNumThreads(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM num_threads_nif = argv[1];
    int num_threads = -1;
    NifResInterpreterBuilder * self_res;
    if (enif_get_resource(env, self_nif, NifResInterpreterBuilder::type, (void **)&self_res) &&
        erlang::nif::get(env, num_threads_nif, &num_threads)) {
        if (self_res->val) {
            auto status = self_res->val->SetNumThreads(num_threads);
            return tflite_status_to_erl_term(env, status);
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}
