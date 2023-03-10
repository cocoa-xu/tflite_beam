#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "tensorflow/lite/c/c_api_types.h"
#include "tflite_status.h"

ERL_NIF_TERM tflite_status_to_erl_term(ErlNifEnv *env, const TfLiteStatus status) {
    switch (status) {
        case kTfLiteOk:
            return erlang::nif::ok(env);
        case kTfLiteError:
            return erlang::nif::error(env, "General runtime error");
        case kTfLiteDelegateError:
            return erlang::nif::error(env, "TfLiteDelegate");
        case kTfLiteApplicationError:
            return erlang::nif::error(env, "Application");
        case kTfLiteDelegateDataNotFound:
            return erlang::nif::error(env, "DelegateDataNotFound");
        case kTfLiteDelegateDataWriteError:
            return erlang::nif::error(env, "DelegateDataWriteError");
        case kTfLiteDelegateDataReadError:
            return erlang::nif::error(env, "DelegateDataReadError");
        default:
            return erlang::nif::error(env, "unknown error");
    }
}
