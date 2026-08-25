#ifndef TFLITE_OPS_BUILTIN_BUILTINRESOLVER_BINDINGS_H
#define TFLITE_OPS_BUILTIN_BUILTINRESOLVER_BINDINGS_H

#pragma once

#include <erl_nif.h>

ERL_NIF_TERM ops_builtin_builtin_resolver_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_OPS_BUILTIN_BUILTINRESOLVER_BINDINGS_H
