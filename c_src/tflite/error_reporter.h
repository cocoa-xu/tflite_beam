#ifndef TFLITE_ERROR_REPORTER_BINDINGS_H
#define TFLITE_ERROR_REPORTER_BINDINGS_H

#pragma once

#include "tensorflow/lite/core/api/error_reporter.h"
#include "../erlang_nif_resource.hpp"

using NifResErrorReporter = erlang_nif_res<tflite::ErrorReporter *>;

ERL_NIF_TERM errorReporter_DefaultErrorReporter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_VERIFIER_BINDINGS_H
