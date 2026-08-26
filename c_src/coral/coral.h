#ifndef TFLITE_CORAL_BINDINGS_H
#define TFLITE_CORAL_BINDINGS_H

#include <vector>
#include <string.h>
#include <erl_nif.h>
#include "../nif_utils.hpp"

#include "tflite/public/edgetpu.h"
#include "tflite/interpreter.h"
#include "tflite/model.h"

#pragma once

ERL_NIF_TERM coral_contains_edgetpu_custom_op(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM coral_edgetpu_devices(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM coral_get_edgetpu_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM coral_get_edgetpu_context_options(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM coral_make_edgetpu_interpreter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM coral_dequantize_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_CORAL_BINDINGS_H
