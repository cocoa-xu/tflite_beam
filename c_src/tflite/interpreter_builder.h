#ifndef TFLITE_INTERPRETER_BUILDER_BINDINGS_H
#define TFLITE_INTERPRETER_BUILDER_BINDINGS_H

#pragma once

#include <erl_nif.h>

ERL_NIF_TERM interpreter_builder_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_builder_build(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM interpreter_builder_set_num_threads(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_INTERPRETER_BUILDER_BINDINGS_H
