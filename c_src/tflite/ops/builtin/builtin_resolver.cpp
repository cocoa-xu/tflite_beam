#include <erl_nif.h>
#include "../../../nif_utils.hpp"
#include "../../../erlang_nif_resource.hpp"
#include "../../../helper.h"

#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/kernels/builtin_op_kernels.h"

#include "builtin_resolver.h"

ERL_NIF_TERM ops_builtin_builtinResolver_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    NifResBuiltinOpResolver * res = nullptr;
    if ((res = alloc_resource_NifResBuiltinOpResolver())) {
        res->val = new tflite::ops::builtin::BuiltinOpResolver();
        ERL_NIF_TERM ret = enif_make_resource(env, res);
        enif_release_resource(res);
        return erlang::nif::ok(env, ret);
    } else {
        return erlang::nif::error(env, "cannot allocate memory for resource");
    }
}
