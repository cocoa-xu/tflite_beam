#ifndef TFLITE_DELEGATE_BINDINGS_H
#define TFLITE_DELEGATE_BINDINGS_H

#pragma once

#include <erl_nif.h>

ERL_NIF_TERM delegate_available(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#ifdef TFLITE_BEAM_XNNPACK_ENABLED
ERL_NIF_TERM delegate_xnnpack_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
#endif

#endif // TFLITE_DELEGATE_BINDINGS_H
