#ifndef TFLITE_TFLITESTATUS_BINDINGS_H
#define TFLITE_TFLITESTATUS_BINDINGS_H

#pragma once

#include <erl_nif.h>
#include "tensorflow/lite/c/c_api_types.h"

ERL_NIF_TERM tflite_status_to_erl_term(ErlNifEnv *env, const TfLiteStatus status);

#endif // TFLITE_TFLITETENSOR_BINDINGS_H
