#ifndef TFLITE_TFLITE_BINDINGS_H
#define TFLITE_TFLITE_BINDINGS_H

#pragma once

#include <erl_nif.h>

ERL_NIF_TERM tflite_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM tflite_runtime_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM tflite_extension_apis_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM tflite_schema_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM xnnpack_max_tensor_dims(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM tflite_print_interpreter_state(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM tflite_reset_variable_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_TFLITE_BINDINGS_H
