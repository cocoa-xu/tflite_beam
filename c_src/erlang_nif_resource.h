#ifndef TFLITE_BEAM_ERLANG_NIF_RESOURCE_H
#define TFLITE_BEAM_ERLANG_NIF_RESOURCE_H

#pragma once

#include <atomic>
#include <memory>
#include <string>
#include <erl_nif.h>

#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"
#include "tensorflow/lite/core/api/error_reporter.h"
#include "tensorflow/lite/interpreter_builder.h"
#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/kernels/builtin_op_kernels.h"
#include "tensorflow/lite/model.h"
#include "tensorflow/lite/stderr_reporter.h"

struct NifResBuiltinOpResolver {
    tflite::ops::builtin::BuiltinOpResolver * val;

    static ErlNifResourceType * type;
    static NifResBuiltinOpResolver * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResBuiltinOpResolver * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResErrorReporter {
    tflite::ErrorReporter * val;
    std::atomic_bool is_default;

    static ErlNifResourceType * type;
    static NifResErrorReporter * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResErrorReporter * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResFlatBufferModel {
    tflite::FlatBufferModel * val;

    // copy the buffer when build from buffer
    const char * copied_buffer;

    static ErlNifResourceType * type;
    static NifResFlatBufferModel * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResFlatBufferModel * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResInterpreterBuilder {
    tflite::InterpreterBuilder * val;
    // kept alive with enif_keep_resource for as long as this builder holds them
    NifResBuiltinOpResolver * op_resolver;
    NifResFlatBufferModel * flatbuffer_model;

    static ErlNifResourceType * type;
    static NifResInterpreterBuilder * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResInterpreterBuilder * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResTfLiteTensor;
struct NifResEdgeTpuContext;
struct NifResInterpreter {
    tflite::Interpreter * val;
    // kept alive with enif_keep_resource; the interpreter reads the model's buffer
    NifResFlatBufferModel * flatbuffer_model;
    // kept alive the same way; an Edge TPU interpreter delegates to this context.
    // nullptr for interpreters that do not run on a TPU
    NifResEdgeTpuContext * edgetpu_context;
    std::map<int, NifResTfLiteTensor *> * tensors;

    static ErlNifResourceType * type;
    static NifResInterpreter * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResInterpreter * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResTfLiteTensor {
    TfLiteTensor * val;
    std::atomic_bool borrowed;
    std::atomic_bool interpreter_has_gone;

    static ErlNifResourceType * type;
    static NifResTfLiteTensor * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResTfLiteTensor * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

#ifdef CORAL_SUPPORT_ENABLED

#include "tflite/public/edgetpu.h"

struct NifResEdgeTpuContext {
    edgetpu::EdgeTpuContext * val;
    // owns the context: the device is handed back once every resource sharing it is gone
    std::shared_ptr<edgetpu::EdgeTpuContext> * context;

    static ErlNifResourceType * type;
    static NifResEdgeTpuContext * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResEdgeTpuContext * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

#endif

#endif //TFLITE_BEAM_ERLANG_NIF_RESOURCE_HPP
