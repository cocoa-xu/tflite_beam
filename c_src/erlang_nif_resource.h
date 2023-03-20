#ifndef TFLITE_ELIXIR_ERLANG_NIF_RESOURCE_H
#define TFLITE_ELIXIR_ERLANG_NIF_RESOURCE_H

#pragma once

#include <atomic>
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
    // OpResolver must live as long as its InterpreterBuilder
    std::atomic_int64_t reference_count{0};
    std::atomic_bool dropped_in_erlang{false};
    std::atomic_bool deleted{false};

    static ErlNifResourceType * type;
    static NifResBuiltinOpResolver * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResBuiltinOpResolver * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResErrorReporter {
    tflite::ErrorReporter * val;
    std::atomic_bool is_default{false};

    static ErlNifResourceType * type;
    static NifResErrorReporter * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResErrorReporter * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResFlatBufferModel {
    tflite::FlatBufferModel * val;
    // FlatBufferModel must live as long as its Interpreter
    std::atomic_int64_t reference_count{0};
    std::atomic_bool dropped_in_erlang{false};
    std::atomic_bool deleted{false};

    static ErlNifResourceType * type;
    static NifResFlatBufferModel * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResFlatBufferModel * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResInterpreterBuilder {
    tflite::InterpreterBuilder * val;
    NifResBuiltinOpResolver * op_resolver;
    NifResFlatBufferModel * flatbuffer_model;

    static ErlNifResourceType * type;
    static NifResInterpreterBuilder * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResInterpreterBuilder * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResInterpreter {
    tflite::Interpreter * val;
    NifResFlatBufferModel * flatbuffer_model;

    static ErlNifResourceType * type;
    static NifResInterpreter * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResInterpreter * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

struct NifResTfLiteTensor {
    TfLiteTensor * val;
    std::atomic_bool borrowed{false};

    static ErlNifResourceType * type;
    static NifResTfLiteTensor * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResTfLiteTensor * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

#ifdef CORAL_SUPPORT_ENABLED

#include "tflite/public/edgetpu.h"

struct NifResEdgeTpuContext {
    edgetpu::EdgeTpuContext * val;

    static ErlNifResourceType * type;
    static NifResEdgeTpuContext * allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error);
    static NifResEdgeTpuContext * get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error);
    static void destruct_resource(ErlNifEnv *env, void *args);
};

#endif

#endif //TFLITE_ELIXIR_ERLANG_NIF_RESOURCE_HPP
