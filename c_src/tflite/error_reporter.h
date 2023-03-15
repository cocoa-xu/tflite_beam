#ifndef TFLITE_ERROR_REPORTER_BINDINGS_H
#define TFLITE_ERROR_REPORTER_BINDINGS_H

#pragma once

#include "tensorflow/lite/core/api/error_reporter.h"
#include "../erlang_nif_resource.hpp"

using NifResErrorReporter = erlang_nif_res<tflite::ErrorReporter *>;

/// @brief Get error reporter
/// @param env [in] NIF env 
/// @param error_reporter_term [in] NIF term that stores the error reporter resource or `nil`
/// @param error_reporter_res [out] resource
/// @param error_reporter [out] pointer to the error reporter
/// @param error_term [out] an error tuple will be set if any error occurred
/// @return true if sucessfully get the error reporter, otherwise false
bool _get_error_reporter(ErlNifEnv *env, ERL_NIF_TERM error_reporter_term, NifResErrorReporter *&error_reporter_res, tflite::ErrorReporter * &error_reporter, ERL_NIF_TERM &error_term);

ERL_NIF_TERM errorReporter_DefaultErrorReporter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_VERIFIER_BINDINGS_H
