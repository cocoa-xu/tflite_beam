#ifdef TFLITE_BEAM_LITERT_API_ENABLED
#include <erl_nif.h>

#include "../nif_utils.hpp"
#include "litert/c/litert_common.h"

// The first thing here calls into LiteRT's own C API rather than the tflite one
// underneath it. It exists to prove the two are linked together and callable:
// a static library nothing references is dropped by the linker, so a build that
// merely compiles says nothing about whether the API is reachable.
ERL_NIF_TERM litert_api_status_string(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    int status;
    if (!enif_get_int(env, argv[0], &status)) {
        return erlang::nif::error(env, "expecting the status to be an integer");
    }

    const char * text = LiteRtGetStatusString(static_cast<LiteRtStatus>(status));
    if (text == nullptr) {
        return erlang::nif::error(env, "LiteRT gave no text for that status");
    }
    return erlang::nif::ok(env, erlang::nif::make_binary(env, text));
}
#endif
