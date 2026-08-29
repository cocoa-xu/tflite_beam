#ifndef TFLITE_BEAM_LITERT_REAPER_H
#define TFLITE_BEAM_LITERT_REAPER_H

#ifdef TFLITE_BEAM_LITERT_API_ENABLED

#include <cstddef>
#include <vector>
#include <erl_nif.h>
#include "litert/c/litert_compiled_model.h"
#include "litert/c/litert_environment.h"
#include "litert/c/litert_model.h"
#include "litert/c/litert_options.h"
#include "litert/c/litert_tensor_buffer.h"

// Everything a compiled model owns, taken out of the resource so the resource
// can be freed at once and the tearing down can happen somewhere it is allowed
// to be slow.
struct LiteRtTeardown {
    std::vector<LiteRtTensorBuffer> * inputs = nullptr;
    std::vector<LiteRtTensorBuffer> * outputs = nullptr;
    std::vector<size_t> * input_sizes = nullptr;
    std::vector<size_t> * output_sizes = nullptr;
    LiteRtCompiledModel compiled = nullptr;
    LiteRtOptions options = nullptr;
    LiteRtModel model = nullptr;
    void * environment = nullptr;   // a resource, released rather than destroyed
    ErlNifMutex * lock = nullptr;
};

// Runs the teardown on the calling thread. Exposed because unload has to finish
// the queue itself once the reaper is gone.
void litert_teardown_now(const LiteRtTeardown & job);

// Hands the teardown to the reaper thread, or does it here if there is no
// reaper, so a failure to start one costs latency and never a leak.
void litert_reaper_submit(const LiteRtTeardown & job);

int litert_reaper_start();
void litert_reaper_stop();

#endif  // TFLITE_BEAM_LITERT_API_ENABLED
#endif  // TFLITE_BEAM_LITERT_REAPER_H
