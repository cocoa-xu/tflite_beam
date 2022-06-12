#ifndef TFLITE_CORAL_BINDINGS_H
#define TFLITE_CORAL_BINDINGS_H

#include "coral/tflite_utils.h"

#pragma once

static ERL_NIF_TERM coral_contains_edgetpu_custom_op(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;
    if (enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res)) {
        if (self_res->val) {
            if (coral::ContainsEdgeTpuCustomOp(*self_res->val)) {
                return erlang::nif::atom(env, "true");
            } else {
                return erlang::nif::atom(env, "false");
            }
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

#endif // TFLITE_CORAL_BINDINGS_H
