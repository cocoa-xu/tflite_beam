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
#include "tflite/c/c_api.h"
#include "tflite/c/common.h"
#include "tflite/interpreter.h"
#include "tflite/kernels/register.h"
#include "tflite/model.h"
#include "nif_utils.hpp"
#include "nif_guard.hpp"
#include "helper.h"

#ifdef TFLITE_BEAM_LITERT_API_ENABLED
extern ERL_NIF_TERM litert_environment_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_compiled_model_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_compiled_model_run(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_compiled_model_fully_accelerated(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_compiled_model_profile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_compiled_model_reset_profile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_compiled_model_io_sizes(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_model_signatures(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_compiled_model_run_with_metrics(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_platform_support(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM nif_litert_call_count(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_compiled_model_set_controlling_process(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM litert_compiled_model_get_controlling_process(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
#endif
extern ERL_NIF_TERM nif_arm_fault(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#ifdef __GNUC__
#  pragma GCC diagnostic ignored "-Wunused-parameter"
#  pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#  pragma GCC diagnostic ignored "-Wunused-variable"
#  pragma GCC diagnostic ignored "-Wunused-function"
#endif

#include "elitte/error_reporter.h"
#include "elitte/flatbuffer_model.h"
#include "elitte/ops/builtin/builtin_resolver.h"
#include "elitte/delegate.h"
#include "elitte/interpreter_builder.h"
#include "elitte/interpreter.h"
#include "elitte/signature_runner.h"
#include "elitte/status.h"
#include "elitte/tflite.h"
#include "elitte/tflitetensor.h"

ErlNifResourceType * NifResBuiltinOpResolver::type = nullptr;
ErlNifResourceType * NifResInterpreterBuilder::type = nullptr;
ErlNifResourceType * NifResFlatBufferModel::type = nullptr;
ErlNifResourceType * NifResInterpreter::type = nullptr;
ErlNifResourceType * NifResErrorReporter::type = nullptr;
ErlNifResourceType * NifResSignatureRunner::type = nullptr;
ErlNifResourceType * NifResTfLiteTensor::type = nullptr;
ErlNifResourceType * NifResDelegate::type = nullptr;
#ifdef TFLITE_BEAM_LITERT_API_ENABLED
ErlNifResourceType * NifResLiteRtEnvironment::type = nullptr;
ErlNifResourceType * NifResLiteRtCompiledModel::type = nullptr;
#endif

#ifdef CORAL_SUPPORT_ENABLED

#include "tflite/public/edgetpu.h"
#include "coral/coral.h"
ErlNifResourceType * NifResEdgeTpuContext::type = nullptr;

#endif

static ERL_NIF_TERM not_compiled(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return erlang::nif::error(env, "Coral support is disabled when compiling this library. Please enable Coral support and recompile this library.");
}

static ERL_NIF_TERM not_compiled_litert(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return erlang::nif::error(env, "the LiteRT API was not compiled into this build. Rebuild with TFLITE_BEAM_ENABLE_LITERT_API=ON to use it.");
}

static ERL_NIF_TERM not_compiled_delegate(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return erlang::nif::error(env, "this delegate was not compiled into this build. tflite_beam_delegate:available/0 lists the ones that were.");
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

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "SignatureRunner", NifResSignatureRunner::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResSignatureRunner::type = rt;

#ifdef TFLITE_BEAM_LITERT_API_ENABLED
    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "LiteRtEnvironment", NifResLiteRtEnvironment::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResLiteRtEnvironment::type = rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "LiteRtCompiledModel", NifResLiteRtCompiledModel::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResLiteRtCompiledModel::type = rt;
#endif

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "TfLiteTensor", NifResTfLiteTensor::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResTfLiteTensor::type = rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "ErrorReporter", NifResErrorReporter::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResErrorReporter::type = rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "Delegate", NifResDelegate::destruct_resource, ERL_NIF_RT_CREATE, NULL);
    if (!rt) return -1;
    NifResDelegate::type = rt;

#ifdef CORAL_SUPPORT_ENABLED
    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "EdgeTpuContext", NifResEdgeTpuContext::destruct_resource, ERL_NIF_RT_CREATE, NULL);
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

// Every entry goes through the exception guard; see nif_guard.hpp. The two
// not_compiled stand-ins do not, because they only build an error term.
#define F(NAME, ARITY) {#NAME, ARITY, erlang::nif::guarded<NAME>, 0}
#define F_CPU(NAME, ARITY) {#NAME, ARITY, erlang::nif::guarded<NAME>, ERL_NIF_DIRTY_JOB_CPU_BOUND}
#define F_IO(NAME, ARITY) {#NAME, ARITY, erlang::nif::guarded<NAME>, ERL_NIF_DIRTY_JOB_IO_BOUND}
#define F_NOT_COMPILED(FAKE_AS, ARITY) {#FAKE_AS, ARITY, not_compiled, 0}
#define F_NOT_COMPILED_DELEGATE(FAKE_AS, ARITY) {#FAKE_AS, ARITY, not_compiled_delegate, 0}
// Without these the Erlang stubs stay unregistered and raise, so on the default
// build every LiteRT call throws where its siblings return {error, Binary}. A
// wrapper cannot tell "not built" from "broken" then, and neither can a user.
#define F_NOT_COMPILED_LITERT(FAKE_AS, ARITY) {#FAKE_AS, ARITY, not_compiled_litert, 0}

static ErlNifFunc nif_functions[] = {
    // For the test suite; see fault_inject.hpp.
    F(nif_arm_fault, 1),
#ifdef TFLITE_BEAM_LITERT_API_ENABLED
    F(nif_litert_call_count, 0),
#endif

    F(error_reporter_default_error_reporter, 0),

    F_IO(flatbuffer_model_build_from_file, 2),
    F_IO(flatbuffer_model_verify_and_build_from_file, 2),
    F_CPU(flatbuffer_model_build_from_buffer, 2),
    F_CPU(flatbuffer_model_verify_and_build_from_buffer, 2),
    F(flatbuffer_model_initialized, 1),
    F(flatbuffer_model_error_reporter, 1),
    F(flatbuffer_model_get_minimum_runtime, 1),
    F_CPU(flatbuffer_model_read_all_metadata, 1),

    F(ops_builtin_builtin_resolver_new, 1),

    F(interpreter_builder_new, 2),
    F_CPU(interpreter_builder_build, 2),
    F(interpreter_builder_set_num_threads, 2),
    F(interpreter_builder_add_delegate, 3),
    F(interpreter_builder_state, 1),

#ifdef TFLITE_BEAM_LITERT_API_ENABLED
    F_IO(litert_environment_new, 1),
    F_CPU(litert_compiled_model_new, 6),
    F_CPU(litert_compiled_model_run, 2),
    F(litert_compiled_model_fully_accelerated, 1),
    F_CPU(litert_compiled_model_profile, 1),
    F(litert_compiled_model_reset_profile, 1),
    F(litert_compiled_model_io_sizes, 1),
    F_CPU(litert_model_signatures, 2),
    F_CPU(litert_compiled_model_run_with_metrics, 3),
    F(litert_platform_support, 0),
    F(litert_compiled_model_set_controlling_process, 2),
    F(litert_compiled_model_get_controlling_process, 1),
#else
    F_NOT_COMPILED_LITERT(litert_environment_new, 1),
    F_NOT_COMPILED_LITERT(litert_compiled_model_new, 6),
    F_NOT_COMPILED_LITERT(litert_compiled_model_run, 2),
    F_NOT_COMPILED_LITERT(litert_compiled_model_fully_accelerated, 1),
    F_NOT_COMPILED_LITERT(litert_compiled_model_profile, 1),
    F_NOT_COMPILED_LITERT(litert_compiled_model_reset_profile, 1),
    F_NOT_COMPILED_LITERT(litert_compiled_model_io_sizes, 1),
    F_NOT_COMPILED_LITERT(litert_model_signatures, 2),
    F_NOT_COMPILED_LITERT(litert_compiled_model_run_with_metrics, 3),
    F_NOT_COMPILED_LITERT(litert_platform_support, 0),
    F_NOT_COMPILED_LITERT(litert_compiled_model_set_controlling_process, 2),
    F_NOT_COMPILED_LITERT(litert_compiled_model_get_controlling_process, 1),
#endif
    F(delegate_available, 0),
    F_IO(delegate_external_new, 2),
#ifdef TFLITE_BEAM_XNNPACK_ENABLED
    F(delegate_xnnpack_new, 3),
#else
    F_NOT_COMPILED_DELEGATE(delegate_xnnpack_new, 3),
#endif

    F(interpreter_new, 0),
    F(interpreter_controlling_process, 1),
    F(interpreter_set_controlling_process, 2),
    F(interpreter_set_inputs, 2),
    F(interpreter_set_outputs, 2),
    F(interpreter_enable_cancellation, 1),
    F(interpreter_cancel, 1),
    F(interpreter_release_non_persistent_memory, 1),
    F(interpreter_reset_variable_tensors, 1),
    F(interpreter_subgraphs_size, 1),
    F(interpreter_get_allow_fp16_precision_for_fp32, 1),
    F(interpreter_set_allow_fp16_precision_for_fp32, 2),
    F(interpreter_signature_inputs, 2),
    F(interpreter_signature_outputs, 2),
    F(interpreter_get_subgraph_index_from_signature, 2),
    F(interpreter_resize_input_tensor, 3),
    F(interpreter_resize_input_tensor_strict, 3),
    F(interpreter_set_variables, 2),
    F(interpreter_inputs, 1),
    F(interpreter_get_input_name, 2),
    F(interpreter_outputs, 1),
    F(interpreter_variables, 1),
    F(interpreter_get_output_name, 2),
    F(interpreter_tensors_size, 1),
    F(interpreter_nodes_size, 1),
    F(interpreter_execution_plan, 1),
    F(interpreter_tensor, 2),
    F(interpreter_signature_keys, 1),
    F_CPU(interpreter_input_tensor, 3),
    F_CPU(interpreter_output_tensor, 2),
    F_CPU(interpreter_allocate_tensors, 1),
    F_CPU(interpreter_invoke, 1),
    F(interpreter_set_num_threads, 2),
    F(interpreter_get_signature_defs, 1),
    // applies lazy delegate providers on first use, which can rewrite and
    // repartition every subgraph, so its cost is the model's and not a fixed one
    F_CPU(interpreter_get_signature_runner, 2),
    F(signature_runner_signature_key, 1),
    F(signature_runner_input_size, 1),
    F(signature_runner_output_size, 1),
    F(signature_runner_input_names, 1),
    F(signature_runner_output_names, 1),
    F(signature_runner_resize_input_tensor, 3),
    F(signature_runner_resize_input_tensor_strict, 3),
    F(signature_runner_cancel, 1),
    F_CPU(signature_runner_input_tensor, 3),
    F_CPU(signature_runner_output_tensor, 2),
    F_CPU(signature_runner_allocate_tensors, 1),
    F_CPU(signature_runner_invoke, 1),

    F(tflitetensor_type, 1),
    F(tflitetensor_dims, 1),
    F(tflitetensor_quantization_params, 1),
    // Both copy the whole tensor, so their cost is the model's rather than a
    // fixed one: measured on this machine a 64 MB tensor takes 3.85 ms to read
    // and 1.57 ms to write, against the millisecond a normal scheduler is meant
    // to see. The three above them only read a field and stay where they are.
    F_CPU(tflitetensor_to_binary, 2),
    F_CPU(tflitetensor_set_data, 2),

    F(tflite_source_tree, 0),
    F(tflite_version, 0),
    F(tensorflow_version, 0),
    F(tflite_runtime_version, 0),
    F(tflite_extension_apis_version, 0),
    F(tflite_schema_version, 0),
    F(xnnpack_max_tensor_dims, 0),

    F_IO(tflite_print_interpreter_state, 1),
    F_CPU(tflite_reset_variable_tensor, 1),

    /* ======= Coral ======= */
#ifdef CORAL_SUPPORT_ENABLED
    F(coral_contains_edgetpu_custom_op, 1),
    F_IO(coral_edgetpu_devices, 0),
    F(coral_get_edgetpu_context, 2),
    F(coral_get_edgetpu_context_options, 1),
    F_IO(coral_make_edgetpu_interpreter, 2),
    F_CPU(coral_dequantize_tensor, 3)
#else
    F_NOT_COMPILED(coral_contains_edgetpu_custom_op, 1),
    F_NOT_COMPILED(coral_edgetpu_devices, 0),
    F_NOT_COMPILED(coral_get_edgetpu_context, 2),
    F_NOT_COMPILED(coral_get_edgetpu_context_options, 1),
    F_NOT_COMPILED(coral_make_edgetpu_interpreter, 2),
    F_NOT_COMPILED(coral_dequantize_tensor, 3)
#endif
};

ERL_NIF_INIT(tflite_beam_nif, nif_functions, on_load, on_reload, on_upgrade, NULL);
