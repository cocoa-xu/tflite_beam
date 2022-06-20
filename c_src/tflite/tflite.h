#ifndef TFLITE_TFLITE_BINDINGS_H
#define TFLITE_TFLITE_BINDINGS_H

#pragma once

static ERL_NIF_TERM tflite_printInterpreterState(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM interpreter_nif = argv[0];
    erlang_nif_res<tflite::Interpreter *> * interpreter_res;
    if (enif_get_resource(env, interpreter_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **)&interpreter_res)) {
        if (interpreter_res->val) {
            tflite::PrintInterpreterState(interpreter_res->val);
            return erlang::nif::atom(env, "nil");
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

#endif // TFLITE_TFLITE_BINDINGS_H
