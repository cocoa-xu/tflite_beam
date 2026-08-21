#include <erl_nif.h>
#include "../../../nif_utils.hpp"
#include "../../../erlang_nif_resource.h"
#include "../../../helper.h"

#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/kernels/builtin_op_kernels.h"

#include "builtin_resolver.h"

ERL_NIF_TERM ops_builtin_builtin_resolver_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    bool apply_default_delegates;
    if (!erlang::nif::get(env, argv[0], &apply_default_delegates)) {
        return erlang::nif::error(env, "expecting apply_default_delegates to be a boolean");
    }

    NifResBuiltinOpResolver * res = nullptr;
    ERL_NIF_TERM ret;

    if (!(res = NifResBuiltinOpResolver::allocate_resource(env, ret))) {
        return ret;
    }
    ResourceRef<NifResBuiltinOpResolver> hold(res);

    // The subclass adds no data members and clears only the delegate creators,
    // which MutableOpResolver owns, and ~OpResolver is virtual -- so it is safe
    // to keep in the same BuiltinOpResolver * with no change to the destructor.
    if (apply_default_delegates) {
        res->val = new tflite::ops::builtin::BuiltinOpResolver();
    } else {
        res->val = new tflite::ops::builtin::BuiltinOpResolverWithoutDefaultDelegates();
    }
    res->apply_default_delegates = apply_default_delegates;
    ret = enif_make_resource(env, res);
    return erlang::nif::ok(env, ret);
}
