#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"
#include "../helper.h"

#include "tflite/c/c_api.h"
#include "tflite/c/common.h"
#include "tflite/core/api/tensor_utils.h"
#include "tflite/interpreter.h"
#include "tflite/optional_debug_tools.h"

#include "status.h"

#include "tflite/schema/schema_generated.h"
#include "../xnnpack_limits.h"

#ifndef TFLITE_BEAM_TFLITE_VERSION
#define TFLITE_BEAM_TFLITE_VERSION "unknown"
#endif

#ifndef TFLITE_BEAM_LITERT_VERSION
#define TFLITE_BEAM_LITERT_VERSION "unknown"
#endif

// MultiAxisQuantization is in the schema LiteRT carries and in no TensorFlow
// release this library has ever built against. Naming the type is what makes
// source_tree/0 below a fact rather than a claim: a binary compiled against the
// wrong tree fails here, so it never exists to answer otherwise.
//
// This matters because the wrong tree does not announce itself. LiteRT's CMake
// puts TensorFlow on the include path for its own reasons, so an include of
// tensorflow/lite/interpreter.h resolves, compiles, links, and passes most of a
// test suite while holding a different definition of the class the library was
// built from.
static_assert(sizeof(tflite::MultiAxisQuantization) > 0,
              "this build did not come from LiteRT's tflite subtree");

// Which source tree the shared object in front of you was built from. A release
// that predates the move has no such function, so asking is enough to tell a
// stale precompiled artifact from a current one.
ERL_NIF_TERM tflite_source_tree(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);
    return erlang::nif::atom(env, "litert");
}

// The version of the TfLite sources this was built from, which since the move
// means LiteRT's, because that is where the runtime lives now. TfLiteVersion()
// below cannot answer this: version.h holds a hand-maintained number that
// upstream forgets to bump, so the tree here reports 2.19.0, and two builds from
// different releases are indistinguishable through it. Delegate plugins must
// match the source version, so this is the one to compare.
ERL_NIF_TERM tflite_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);
    return erlang::nif::make_binary(env, TFLITE_BEAM_LITERT_VERSION);
}

// The TensorFlow release the build pulled in. TensorFlow is not where the
// runtime comes from any more: LiteRT reaches into it for compiler/mlir/lite,
// TSL and XLA, and pins a version whose schema its own is meant to agree with.
// Mismatching it links two different definitions of the same tables, so the
// number is worth having on hand when something reads wrong.
ERL_NIF_TERM tensorflow_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

    {
        ERL_NIF_TERM owner_error;
        if (!(interpreter_res = NifResInterpreter::get_resource(env, interpreter_nif, owner_error))) {
            return owner_error;
        }
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
