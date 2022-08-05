#ifndef TFLITE_TFLiteTensor_BINDINGS_H
#define TFLITE_TFLiteTensor_BINDINGS_H

#pragma once

#include <erl_nif.h>
#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"

int _TFLiteTensor_name(ErlNifEnv *env, TFLiteTensor * tensor, ERL_NIF_TERM &out);
int _TFLiteTensor_shape(ErlNifEnv *env, TFLiteTensor * tensor, ERL_NIF_TERM &out);
int _TFLiteTensor_shape_signature(ErlNifEnv *env, TFLiteTensor * tensor, ERL_NIF_TERM &out);
int _TFLiteTensor_type(ErlNifEnv *env, TFLiteTensor * tensor, ERL_NIF_TERM &out);
int _TFLiteTensor_quantization_params(ErlNifEnv *env, TFLiteTensor * tensor, ERL_NIF_TERM &out);
int _TFLiteTensor_sparsity_params(ErlNifEnv *env, TFLiteTensor * tensor, ERL_NIF_TERM &out);

ERL_NIF_TERM TFLiteTensor_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM TFLiteTensor_dims(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM TFLiteTensor_quantization_params(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM TFLiteTensor_to_binary(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM TFLiteTensor_set_data(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_TFLiteTensor_BINDINGS_H
