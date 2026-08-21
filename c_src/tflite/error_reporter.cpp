#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"
#include "../helper.h"

#include "tensorflow/lite/core/api/error_reporter.h"
#include "tensorflow/lite/stderr_reporter.h"

#include "error_reporter.h"

ERL_NIF_TERM error_reporter_default_error_reporter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    auto e = tflite::DefaultErrorReporter();
    
    if (e == nullptr) {
        return erlang::nif::error(env, "cannot get default ErrorReporter");
    }

    ERL_NIF_TERM ret;
    _make_error_reporter(env, e, ret);
    return ret;
}

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

NifResErrorReporter * _make_error_reporter(ErlNifEnv *env, tflite::ErrorReporter * e, ERL_NIF_TERM &out) {
    NifResErrorReporter * res = nullptr;

    if (!(res = NifResErrorReporter::allocate_resource(env, out))) {
        return res;
    }
    ResourceRef<NifResErrorReporter> hold(res);

    res->val = e;
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    out = erlang::nif::ok(env, ret);
    return res;
}
