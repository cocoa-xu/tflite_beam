#ifndef TFLITE_HELPER_H
#define TFLITE_HELPER_H

#pragma once

#include <erl_nif.h>
#include "erlang_nif_resource.h"

bool tensor_type_to_erl_term(const TfLiteType in_type, ErlNifEnv *env, ERL_NIF_TERM &out_term);
bool tensor_type_from_erl_term(ErlNifEnv *env, const ERL_NIF_TERM in_term, TfLiteType &out_type);

#endif  // TFLITE_HELPER_H
