#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"
#include "../helper.h"

#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"
#include "tensorflow/lite/core/api/tensor_utils.h"
#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/optional_debug_tools.h"

#include "status.h"
#include "../xnnpack_limits.h"

#ifndef TFLITE_BEAM_TFLITE_VERSION
#define TFLITE_BEAM_TFLITE_VERSION "unknown"
#endif

// The version of the TfLite sources this was built from. TfLiteVersion() below
// cannot answer that: lite/version.h holds a hand-maintained number that upstream
// forgets to bump, so a 2.21.0 tree reports 2.19.0, and two builds from different
// releases are indistinguishable through it. Delegate plugins must match the
// source version, so this is the one to compare.
ERL_NIF_TERM tflite_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);
    return erlang::nif::make_binary(env, TFLITE_BEAM_TFLITE_VERSION);
}

// What the linked runtime says about itself. Kept for diagnosis, not for
// matching -- see above.
ERL_NIF_TERM tflite_runtime_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);
    return erlang::nif::make_binary(env, TfLiteVersion());
}

ERL_NIF_TERM tflite_extension_apis_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);
    return erlang::nif::make_binary(env, TfLiteExtensionApisVersion());
}

ERL_NIF_TERM tflite_schema_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);
    return enif_make_int(env, TfLiteSchemaVersion());
}

// The widest tensor the attached delegate can describe, or nil when this build
// has no delegate that imposes one. Reshaping a delegated tensor past it is
// refused rather than performed, so this is the number that explains the
// refusal instead of leaving it as an unexplained rule.
ERL_NIF_TERM xnnpack_max_tensor_dims(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);
    if (kMaxDelegatedRank <= 0) {
        return erlang::nif::atom(env, "nil");
    }
    return enif_make_int(env, kMaxDelegatedRank);
}

ERL_NIF_TERM tflite_print_interpreter_state(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM interpreter_nif = argv[0];
    NifResInterpreter * interpreter_res;

    if (!enif_get_resource(env, interpreter_nif, NifResInterpreter::type, (void **)&interpreter_res) || interpreter_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResInterpreter resource");
    }

    // walks the whole interpreter, so it cannot do so while a rebuild is
    // replacing the thing it is walking
    TFLITE_BEAM_INTERPRETER_IN_USE(interpreter_res);

    tflite::PrintInterpreterState(interpreter_res->val);
    return erlang::nif::atom(env, "nil");
}

ERL_NIF_TERM tflite_reset_variable_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM tensor_nif = argv[0];
    NifResTfLiteTensor *self_res = nullptr;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResTfLiteTensor::get_resource(env, tensor_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_BORROWED_IN_USE(self_res, "cannot access NifResTfLiteTensor resource: the handle has been retired, because the interpreter moved its tensors");

    TfLiteStatus status = tflite::ResetVariableTensor(self_res->val);
    return tflite_status_to_erl_term(env, status);
}
