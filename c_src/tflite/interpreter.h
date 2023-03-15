#ifndef TFLITE_INTERPRETER_BINDINGS_H
#define TFLITE_INTERPRETER_BINDINGS_H

#pragma once

#include <erl_nif.h>

ERL_NIF_TERM interpreter_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_allocateTensors(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_inputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_getInputName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_input_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_invoke(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_outputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_getOutputName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_output_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_setNumThreads(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_get_full_signature_list(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_INTERPRETER_BINDINGS_H
