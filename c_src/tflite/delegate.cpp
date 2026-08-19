#include <vector>

#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"

#include "delegate.h"

// What was compiled in, not what the machine has: whether a device is present is
// answered by trying to create the delegate and getting {error, _} back. The two
// questions have different answers on the same binary, so they get different
// functions.
ERL_NIF_TERM delegate_available(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);

    std::vector<ERL_NIF_TERM> available;

    return enif_make_list_from_array(env, available.data(), (unsigned)available.size());
}
