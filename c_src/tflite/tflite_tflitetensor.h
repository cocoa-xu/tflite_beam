#ifndef TFLITE_TFLITETENSOR_BINDINGS_H
#define TFLITE_TFLITETENSOR_BINDINGS_H

#pragma once

#include <erl_nif.h>
#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"

int _tflitetensor_name(ErlNifEnv *env, TfLiteTensor * tensor, ERL_NIF_TERM &out);
int _tflitetensor_shape(ErlNifEnv *env, TfLiteTensor * tensor, ERL_NIF_TERM &out);
int _tflitetensor_shape_signature(ErlNifEnv *env, TfLiteTensor * tensor, ERL_NIF_TERM &out);
int _tflitetensor_type(ErlNifEnv *env, TfLiteTensor * tensor, ERL_NIF_TERM &out);
int _tflitetensor_quantization_params(ErlNifEnv *env, TfLiteTensor * tensor, ERL_NIF_TERM &out);
int _tflitetensor_sparsity_params(ErlNifEnv *env, TfLiteTensor * tensor, ERL_NIF_TERM &out);

ERL_NIF_TERM tflitetensor_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM tflitetensor_dims(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM tflitetensor_quantization_params(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_TFLITETENSOR_BINDINGS_H
