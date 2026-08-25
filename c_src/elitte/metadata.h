#ifndef TFLITE_TFLITESTATUS_BINDINGS_H
#define TFLITE_TFLITESTATUS_BINDINGS_H

#pragma once

#include <erl_nif.h>

ERL_NIF_TERM tflite_metadata_to_erl_term(ErlNifEnv *env, const void *buf, size_t size);

#endif // TFLITE_TFLITETENSOR_BINDINGS_H
