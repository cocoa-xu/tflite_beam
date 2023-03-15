#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.hpp"
#include "../helper.h"

#include "tensorflow/lite/core/api/error_reporter.h"
#include "tensorflow/lite/stderr_reporter.h"

#include "error_reporter.h"

bool _get_error_reporter(ErlNifEnv *env, ERL_NIF_TERM error_reporter_term, NifResErrorReporter *&error_reporter_res, tflite::ErrorReporter * &error_reporter, ERL_NIF_TERM &error_term) {
    if (enif_get_resource(env, error_reporter_term, NifResErrorReporter::type, (void **)&error_reporter_res) && error_reporter_res->val) {
        error_reporter = error_reporter_res->val;
        return true;
    } else if (erlang::nif::check_nil(env, error_reporter_term)) {
        error_reporter = tflite::DefaultErrorReporter();
        return true;
    } else {
        error_term = erlang::nif::error(env, "Invalid value for error_reporter");
        return false;
    }
}

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
