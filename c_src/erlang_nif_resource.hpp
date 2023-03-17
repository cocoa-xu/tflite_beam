#ifndef TFLITE_ELIXIR_ERLANG_NIF_RESOURCE_HPP
#define TFLITE_ELIXIR_ERLANG_NIF_RESOURCE_HPP

#pragma once

#include <atomic>
#include <erl_nif.h>
#include "tensorflow/lite/interpreter_builder.h"
#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/kernels/builtin_op_kernels.h"
#include "tensorflow/lite/model.h"

template<typename R>
struct erlang_nif_res {
    R val;
    int peak;
    static ErlNifResourceType * type;
};
template<typename R> ErlNifResourceType * erlang_nif_res<R>::type = nullptr;

struct NifResBuiltinOpResolver {
    tflite::ops::builtin::BuiltinOpResolver * val;
    // OpResolver must live as long as its InterpreterBuilder
    std::atomic_int64_t reference_count{0};
    std::atomic_bool dropped_in_erlang{false};
    static ErlNifResourceType * type;
};

struct NifResFlatBufferModel {
    tflite::FlatBufferModel * val;
    // FlatBufferModel must live as long as its Interpreter
    std::atomic_int64_t reference_count{0};
    std::atomic_bool dropped_in_erlang{false};
    static ErlNifResourceType * type;
};

struct NifResInterpreterBuilder {
    tflite::InterpreterBuilder * val;
    NifResBuiltinOpResolver * op_resolver;
    NifResFlatBufferModel * flatbuffer_model;
    static ErlNifResourceType * type;
};

struct NifResInterpreter {
    tflite::Interpreter * val;
    NifResFlatBufferModel * flatbuffer_model;
    static ErlNifResourceType * type;
};

#endif //TFLITE_ELIXIR_ERLANG_NIF_RESOURCE_HPP
