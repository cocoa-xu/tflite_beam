#ifndef TFLITE_VERIFIER_BINDINGS_H
#define TFLITE_VERIFIER_BINDINGS_H

#pragma once

#include "tensorflow/lite/core/api/verifier.h"
#include "../erlang_nif_resource.hpp"

using NifResVerifier = erlang_nif_res<tflite::TfLiteVerifier *>;

#endif // TFLITE_VERIFIER_BINDINGS_H
