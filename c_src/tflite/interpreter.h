#ifndef TFLITE_INTERPRETER_BINDINGS_H
#define TFLITE_INTERPRETER_BINDINGS_H

#pragma once

#include <erl_nif.h>

ERL_NIF_TERM interpreter_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_set_inputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_allocate_tensors(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_inputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_get_input_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_input_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_invoke(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_outputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_get_output_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_output_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_set_num_threads(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_get_signature_defs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_INTERPRETER_BINDINGS_H
