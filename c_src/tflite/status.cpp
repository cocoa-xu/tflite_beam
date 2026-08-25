#include <erl_nif.h>
#include "../nif_utils.hpp"

#include "tflite/c/c_api_types.h"

#include "status.h"

// TfLite reports why a status happened through its error reporter, which writes
// to stderr; only the status code reaches us. These say what the code means and,
// where the detail is what the caller actually needs, point at where it went.
ERL_NIF_TERM tflite_status_to_erl_term(ErlNifEnv *env, const TfLiteStatus status) {
    switch (status) {
        case kTfLiteOk:
            return erlang::nif::ok(env);
        case kTfLiteError:
            return erlang::nif::error(env, "runtime error; TfLite logged the reason to stderr");
        case kTfLiteDelegateError:
            return erlang::nif::error(env, "the delegate failed; TfLite logged the reason to stderr");
        case kTfLiteApplicationError:
            return erlang::nif::error(env, "the delegate is incompatible with this interpreter's graph and was not applied");
        case kTfLiteDelegateDataNotFound:
            return erlang::nif::error(env, "serialized delegate data not found");
        case kTfLiteDelegateDataWriteError:
            return erlang::nif::error(env, "cannot write serialized delegate data");
        case kTfLiteDelegateDataReadError:
            return erlang::nif::error(env, "cannot read serialized delegate data");
        case kTfLiteUnresolvedOps:
            return erlang::nif::error(env, "the model uses ops that are not registered in this build");
        case kTfLiteCancelled:
            return erlang::nif::error(env, "cancelled");
        case kTfLiteOutputShapeNotKnown:
            return erlang::nif::error(env, "the output shape cannot be determined");
        default:
            return erlang::nif::error(env, "unknown error");
    }
}
