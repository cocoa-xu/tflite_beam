#ifndef TFLITE_DELEGATE_BINDINGS_H
#define TFLITE_DELEGATE_BINDINGS_H

#pragma once

#include <erl_nif.h>

ERL_NIF_TERM delegate_available(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_DELEGATE_BINDINGS_H
