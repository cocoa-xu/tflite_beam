#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.hpp"
#include "../helper.h"

#include "tensorflow/lite/core/api/error_reporter.h"
#include "tensorflow/lite/stderr_reporter.h"

#include "error_reporter.h"

ERL_NIF_TERM errorReporter_DefaultErrorReporter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    NifResErrorReporter * res;
    auto e = tflite::DefaultErrorReporter();
    if (e != nullptr) {
        if (alloc_resource(&res)) {
            res->val = e;
            ERL_NIF_TERM ret = enif_make_resource(env, res);
            enif_release_resource(res);
            return erlang::nif::ok(env, ret);
        } else {
            return erlang::nif::error(env, "cannot allocate memory for resource");
        }
    } else {
        return erlang::nif::error(env, "cannot get default ErrorReporter");
    }
}
