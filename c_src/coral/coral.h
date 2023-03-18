#ifndef TFLITE_CORAL_BINDINGS_H
#define TFLITE_CORAL_BINDINGS_H

#include <vector>
#include <string.h>
#include <erl_nif.h>
#include "../nif_utils.hpp"

#include "tflite/public/edgetpu.h"
#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/model.h"

#pragma once

void destruct_egdetpu_context(ErlNifEnv *env, void *args);

ERL_NIF_TERM coral_contains_edgetpu_custom_op(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM coral_edgetpu_devices(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM coral_get_edgetpu_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM coral_make_edgetpu_interpreter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM coral_dequantize_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_CORAL_BINDINGS_H
