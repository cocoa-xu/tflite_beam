#ifndef TFLITE_BEAM_LITERT_BACKEND_OPTIONS_H
#define TFLITE_BEAM_LITERT_BACKEND_OPTIONS_H

#include <erl_nif.h>
#include <string>
#include <utility>
#include <vector>

using LiteRtBackendPayloads = std::vector<std::pair<std::string, std::string>>;

LiteRtBackendPayloads litert_backend_payloads(ErlNifEnv *env, ERL_NIF_TERM options, int precision);

#endif
