/* Copyright 2018 The TensorFlow Authors. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
==============================================================================*/
#include <cstdio>
#include <erl_nif.h>
#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"
#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/model.h"
#include "nif_utils.hpp"
#include "helper.h"

#ifdef __GNUC__
#  pragma GCC diagnostic ignored "-Wunused-parameter"
#  pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#  pragma GCC diagnostic ignored "-Wunused-variable"
#  pragma GCC diagnostic ignored "-Wunused-function"
#endif

#include "tflite/error_reporter.h"
#include "tflite/flatbuffer_model.h"
#include "tflite/ops/builtin/builtin_resolver.h"
#include "tflite/interpreter_builder.h"
#include "tflite/interpreter.h"
#include "tflite/status.h"
#include "tflite/tflite.h"
#include "tflite/tflitetensor.h"

ErlNifResourceType * NifResBuiltinOpResolver::type = nullptr;
ErlNifResourceType * NifResInterpreterBuilder::type = nullptr;
ErlNifResourceType * NifResFlatBufferModel::type = nullptr;
ErlNifResourceType * NifResInterpreter::type = nullptr;
ErlNifResourceType * NifResErrorReporter::type = nullptr;
ErlNifResourceType * NifResTfLiteTensor::type = nullptr;

#ifdef CORAL_SUPPORT_ENABLED

#include "tflite/public/edgetpu.h"
#include "coral/coral.h"
ErlNifResourceType * NifResEdgeTpuContext::type = nullptr;

#endif

static ERL_NIF_TERM not_compiled(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return erlang::nif::error(env, "Coral support is disabled when compiling this library. Please enable Coral support and recompile this library.");
}

static int
on_load(ErlNifEnv* env, void**, ERL_NIF_TERM) {
    ErlNifResourceType *rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "BuiltinOpResolver", NifResBuiltinOpResolver::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResBuiltinOpResolver::type = rt;
    
    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "FlatBufferModel", NifResFlatBufferModel::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResFlatBufferModel::type = rt;    

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "InterpreterBuilder", NifResInterpreterBuilder::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResInterpreterBuilder::type = rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "Interpreter", NifResInterpreter::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResInterpreter::type = rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "TfLiteTensor", NifResTfLiteTensor::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResTfLiteTensor::type = rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "ErrorReporter", NifResErrorReporter::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResErrorReporter::type = rt;

#ifdef CORAL_SUPPORT_ENABLED
    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "EdgeTpuContext", destruct_egdetpu_context, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResEdgeTpuContext::type = rt;
#endif

    return 0;
}

static int on_reload(ErlNifEnv*, void**, ERL_NIF_TERM) {
    return 0;
}

static int on_upgrade(ErlNifEnv*, void**, void**, ERL_NIF_TERM) {
    return 0;
}

#define F(NAME, ARITY) {#NAME, ARITY, NAME, 0}
#define F_NOT_COMPILED(FAKE_AS, ARITY) {#FAKE_AS, ARITY, not_compiled, 0}
#define F_CPU(NAME, ARITY) {#NAME, ARITY, NAME, ERL_NIF_DIRTY_JOB_CPU_BOUND}
#define F_IO(NAME, ARITY) {#NAME, ARITY, NAME, ERL_NIF_DIRTY_JOB_IO_BOUND}

static ErlNifFunc nif_functions[] = {
    F(error_reporter_default_error_reporter, 0),

    F_IO(flatbuffer_model_build_from_file, 2),
    F_IO(flatbuffer_model_verify_and_build_from_file, 2),
    F_CPU(flatbuffer_model_build_from_buffer, 2),
    F(flatbuffer_model_initialized, 1),
    F(flatbuffer_model_error_reporter, 1),
    F(flatbuffer_model_get_minimum_runtime, 1),
    F(flatbuffer_model_read_all_metadata, 1),

    F(ops_builtin_builtin_resolver_new, 0),

    F(interpreter_builder_new, 2),
    F(interpreter_builder_build, 2),
    F(interpreter_builder_set_num_threads, 2),

    F(interpreter_new, 0),
    F(interpreter_set_inputs, 2),
    F(interpreter_set_outputs, 2),
    F(interpreter_set_variables, 2),
    F(interpreter_inputs, 1),
    F(interpreter_get_input_name, 2),
    F(interpreter_outputs, 1),
    F(interpreter_variables, 1),
    F(interpreter_get_output_name, 2),
    F(interpreter_tensor, 2),
    F_CPU(interpreter_input_tensor, 3),
    F_CPU(interpreter_output_tensor, 2),
    F(interpreter_allocate_tensors, 1),
    F_CPU(interpreter_invoke, 1),
    F(interpreter_set_num_threads, 2),
    F(interpreter_get_signature_defs, 1),

    F(tflitetensor_type, 1),
    F(tflitetensor_dims, 1),
    F(tflitetensor_quantization_params, 1),
    F(tflitetensor_to_binary, 2),
    F(tflitetensor_set_data, 2),

    F_IO(tflite_print_interpreter_state, 1),
    F_CPU(tflite_reset_variable_tensor, 1),

    /* ======= Coral ======= */
#ifdef CORAL_SUPPORT_ENABLED
    F(coral_contains_edgetpu_custom_op, 1),
    F_IO(coral_edgetpu_devices, 0),
    F(coral_get_edgetpu_context, 2),
    F_IO(coral_make_edgetpu_interpreter, 2),
    F_CPU(coral_dequantize_tensor, 3)
#else
    F_NOT_COMPILED(coral_contains_edgetpu_custom_op, 1),
    F_NOT_COMPILED(coral_edgetpu_devices, 0),
    F_NOT_COMPILED(coral_get_edgetpu_context, 2),
    F_NOT_COMPILED(coral_make_edgetpu_interpreter, 2),
    F_NOT_COMPILED(coral_dequantize_tensor, 3)
#endif
};

ERL_NIF_INIT(Elixir.TFLiteElixir.Nif, nif_functions, on_load, on_reload, on_upgrade, NULL);
