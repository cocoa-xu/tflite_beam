#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.hpp"
#include "../helper.h"

#include "tensorflow/lite/core/api/error_reporter.h"
#include "tensorflow/lite/stderr_reporter.h"

#include "error_reporter.h"

ERL_NIF_TERM errorReporter_DefaultErrorReporter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    NifResErrorReporter * res = nullptr;
    auto e = tflite::DefaultErrorReporter();
    ERL_NIF_TERM ret;
    if (e != nullptr) {
        res = _make_error_reporter(env, e, ret);
        res->is_default = true;
        return ret;
    } else {
        return erlang::nif::error(env, "cannot get default ErrorReporter");
    }
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
    if ((res = alloc_resource_NifResErrorReporter())) {
        res->val = e;
        res->is_default = e == tflite::DefaultErrorReporter();
        ERL_NIF_TERM ret = enif_make_resource(env, res);
        enif_release_resource(res);
        out = erlang::nif::ok(env, ret);
        return res;
    } else {
        out = erlang::nif::error(env, "cannot allocate memory for resource");
        return res;
    }
}
