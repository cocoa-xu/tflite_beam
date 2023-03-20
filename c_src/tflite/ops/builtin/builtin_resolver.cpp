#include <erl_nif.h>
#include "../../../nif_utils.hpp"
#include "../../../erlang_nif_resource.h"
#include "../../../helper.h"

#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/kernels/builtin_op_kernels.h"

#include "builtin_resolver.h"

ERL_NIF_TERM ops_builtin_builtin_resolver_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    NifResBuiltinOpResolver * res = nullptr;
    ERL_NIF_TERM ret;

    if (!(res = NifResBuiltinOpResolver::allocate_resource(env, ret))) {
        return ret;
    }

    res->val = new tflite::ops::builtin::BuiltinOpResolver();
    ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return erlang::nif::ok(env, ret);
}
