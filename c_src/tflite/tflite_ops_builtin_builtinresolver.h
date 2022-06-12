#ifndef TFLITE_OPS_BUILTIN_BUILTINRESOLVER_BINDINGS_H
#define TFLITE_OPS_BUILTIN_BUILTINRESOLVER_BINDINGS_H

#pragma once

static ERL_NIF_TERM ops_builtin_builtinResolver_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    erlang_nif_res<tflite::ops::builtin::BuiltinOpResolver *> * res;
    if (alloc_resource(&res)) {
        res->val = new tflite::ops::builtin::BuiltinOpResolver();
        ERL_NIF_TERM ret = enif_make_resource(env, res);
        enif_release_resource(res);
        return erlang::nif::ok(env, ret);
    } else {
        return erlang::nif::error(env, "cannot allocate memory for resource");
    }
}

#endif // TFLITE_OPS_BUILTIN_BUILTINRESOLVER_BINDINGS_H
