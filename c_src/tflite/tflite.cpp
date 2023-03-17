#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.hpp"
#include "../helper.h"

#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"
#include "tensorflow/lite/core/api/tensor_utils.h"
#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/optional_debug_tools.h"

#include "status.h"

ERL_NIF_TERM tflite_printInterpreterState(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM interpreter_nif = argv[0];
    NifResInterpreter * interpreter_res;
    if (enif_get_resource(env, interpreter_nif, NifResInterpreter::type, (void **)&interpreter_res)) {
        if (interpreter_res->val) {
            tflite::PrintInterpreterState(interpreter_res->val);
            return erlang::nif::atom(env, "nil");
        } else {
            return erlang::nif::error(env, "invalid resource");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

ERL_NIF_TERM tflite_resetVariableTensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM tensor_nif = argv[0];
    erlang_nif_res<TfLiteTensor *> *self_res;
    if (enif_get_resource(env, tensor_nif, erlang_nif_res<TfLiteTensor *>::type, (void **) &self_res)) {
        if (self_res->val) {
            TfLiteStatus status = tflite::ResetVariableTensor(self_res->val);
            return tflite_status_to_erl_term(env, status);
        } else {
            return erlang::nif::error(env, "invalid resource");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}
