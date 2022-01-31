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
#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/model.h"
#include "tensorflow/lite/optional_debug_tools.h"
#include "nif_utils.hpp"
#include <erl_nif.h>

#ifdef __GNUC__
#  pragma GCC diagnostic ignored "-Wunused-parameter"
#  pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#  pragma GCC diagnostic ignored "-Wunused-variable"
#  pragma GCC diagnostic ignored "-Wunused-function"
#endif

template<typename R>
struct erlang_nif_res {
    R val;
    int peak;
    static ErlNifResourceType * type;
};
template<typename R> ErlNifResourceType * erlang_nif_res<R>::type = nullptr;

template<typename R>
int alloc_resource(erlang_nif_res<R> **res) {
    *res = (erlang_nif_res<R> *)enif_alloc_resource(erlang_nif_res<R>::type, sizeof(erlang_nif_res<R>));
    return (*res != nullptr);
}

template <typename T>
static void destruct_raw_ptr(ErlNifEnv *env, void *args) {
    auto res = (erlang_nif_res<T *> *)args;
    if (res->val && !res->peak) {
         delete res->val;
         res->val = nullptr;
    }
}

static bool tensor_type_to_erl_term(const TfLiteType in_type, ErlNifEnv *env, ERL_NIF_TERM &out_term) {
    bool ok = true;
    switch (in_type) {
        case kTfLiteNoType:
            out_term = erlang::nif::atom(env, "no_type");
            break;
        case kTfLiteFloat32:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "s"),
                                    enif_make_int(env, 32));
            break;
        case kTfLiteInt32:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "s"),
                                    enif_make_int(env, 32));
            break;
        case kTfLiteUInt8:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "u"),
                                    enif_make_int(env, 8));
            break;
        case kTfLiteInt64:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "s"),
                                    enif_make_int(env, 64));
            break;
        case kTfLiteString:
            out_term = erlang::nif::atom(env, "string");
            break;
        case kTfLiteBool:
            out_term = erlang::nif::atom(env, "bool");
            break;
        case kTfLiteInt16:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "s"),
                                    enif_make_int(env, 16));
            break;
        case kTfLiteComplex64:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "c"),
                                    enif_make_int(env, 64));
            break;
        case kTfLiteInt8:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "s"),
                                    enif_make_int(env, 8));
            break;
        case kTfLiteFloat16:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "f"),
                                    enif_make_int(env, 16));
            break;
        case kTfLiteFloat64:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "f"),
                                    enif_make_int(env, 64));
            break;
        case kTfLiteComplex128:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "c"),
                                    enif_make_int(env, 128));
            break;
        case kTfLiteUInt64:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "u"),
                                    enif_make_int(env, 64));
            break;
        case kTfLiteResource:
            out_term = erlang::nif::atom(env, "resource");
            break;
        case kTfLiteVariant:
            out_term = erlang::nif::atom(env, "variant");
            break;
        case kTfLiteUInt32:
            out_term = enif_make_tuple2(env,
                                    enif_make_atom(env, "u"),
                                    enif_make_int(env, 32));
            break;
        default:
            ok = false;
    }
    return ok;
}

static bool tensor_type_from_erl_term(ErlNifEnv *env, const ERL_NIF_TERM in_term, TfLiteType &out_type) {
    bool ok = false;
    if (enif_is_tuple(env, in_term)) {
        int arity;
        const ERL_NIF_TERM * array = nullptr;
        if (enif_get_tuple(env, in_term, &arity, &array)) {
            if (arity == 2) {
                std::string data_type;
                int bits;
                if (erlang::nif::get_atom(env, array[0], data_type) &&
                    erlang::nif::get(env, array[1], &bits)) {
                    ok = true;
                    if (data_type == "u") {
                        switch (bits) {
                            case 8:
                                out_type = kTfLiteUInt8;
                                break;
                            case 64:
                                out_type = kTfLiteUInt64;
                                break;
                            case 32:
                                out_type = kTfLiteUInt32;
                                break;
                            default:
                                ok = false;
                        }
                    } else if (data_type == "s") {
                        switch (bits) {
                            case 32:
                                out_type = kTfLiteInt32;
                                break;
                            case 64:
                                out_type = kTfLiteInt64;
                                break;
                            case 16:
                                out_type = kTfLiteInt16;
                                break;
                            case 8:
                                out_type = kTfLiteInt8;
                                break;
                            default:
                                ok = false;
                        }
                    } else if (data_type == "f") {
                        switch (bits) {
                            case 32:
                                out_type = kTfLiteFloat32;
                                break;
                            case 16:
                                out_type = kTfLiteFloat16;
                                break;
                            case 64:
                                out_type = kTfLiteFloat64;
                                break;
                            default:
                                ok = false;
                        }
                    } else if (data_type == "c") {
                        switch (bits) {
                            case 64:
                                out_type = kTfLiteComplex64;
                                break;
                            case 128:
                                out_type = kTfLiteComplex128;
                                break;
                            default:
                                ok = false;
                        }
                    }
                }
            }
        }
    } else if (enif_is_atom(env, in_term)) {
        std::string data_type;
        if (erlang::nif::get_atom(env, in_term, data_type)) {
            ok = true;
            if (data_type == "no_type") {
                out_type = kTfLiteNoType;
            } else if (data_type == "string") {
                out_type = kTfLiteString;
            } else if (data_type == "bool") {
                out_type = kTfLiteBool;
            } else if (data_type == "resource") {
                out_type = kTfLiteResource;
            } else if (data_type == "variant") {
                out_type = kTfLiteVariant;
            } else {
                ok = false;
            }
        }
    }

    return ok;
}

#include "tflite_flatbuffermodel.h"
#include "tflite_ops_builtin_builtinresolver.h"
#include "tflite_interpreter_builder.h"
#include "tflite_interpreter.h"
#include "tflite_tflitetensor.h"
#include "tflite.h"

static int
on_load(ErlNifEnv* env, void**, ERL_NIF_TERM)
{
    ErlNifResourceType *rt;
    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "FlatBufferModel", destruct_raw_ptr<tflite::FlatBufferModel>, ERL_NIF_RT_CREATE, NULL);                                                             \
    if (!rt) return -1;
    erlang_nif_res<tflite::FlatBufferModel *>::type = rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "BuiltinOpResolver", destruct_raw_ptr<tflite::ops::builtin::BuiltinOpResolver>, ERL_NIF_RT_CREATE, NULL);                                                             \
    if (!rt) return -1;
    erlang_nif_res<tflite::ops::builtin::BuiltinOpResolver *>::type = rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "InterpreterBuilder", destruct_raw_ptr<tflite::InterpreterBuilder>, ERL_NIF_RT_CREATE, NULL);                                                             \
    if (!rt) return -1;
    erlang_nif_res<tflite::InterpreterBuilder *>::type = rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "Interpreter", destruct_raw_ptr<tflite::Interpreter>, ERL_NIF_RT_CREATE, NULL);                                                             \
    if (!rt) return -1;
    erlang_nif_res<tflite::Interpreter *>::type = rt;

    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "TfLiteTensor", destruct_raw_ptr<TfLiteTensor>, ERL_NIF_RT_CREATE, NULL);                                                             \
    if (!rt) return -1;
    erlang_nif_res<TfLiteTensor *>::type = rt;

    return 0;
}

static int on_reload(ErlNifEnv*, void**, ERL_NIF_TERM)
{
    return 0;
}

static int on_upgrade(ErlNifEnv*, void**, void**, ERL_NIF_TERM)
{
    return 0;
}

#define F(NAME, ARITY) {#NAME, ARITY, NAME, 0}
#define F_CPU(NAME, ARITY) {#NAME, ARITY, NAME, ERL_NIF_DIRTY_JOB_CPU_BOUND}
#define F_IO(NAME, ARITY) {#NAME, ARITY, NAME, ERL_NIF_DIRTY_JOB_IO_BOUND}

static ErlNifFunc nif_functions[] = {
    F_IO(flatBufferModel_buildFromFile, 1),
    F_CPU(flatBufferModel_buildFromBuffer, 1),
    F(flatBufferModel_initialized, 1),
    F(flatBufferModel_getMinimumRuntime, 1),
    F(flatBufferModel_readAllMetadata, 1),

    F(ops_builtin_builtinResolver_new, 0),

    F(interpreterBuilder_new, 2),
    F(interpreterBuilder_build, 2),

    F(interpreter_new, 0),
    F(interpreter_allocateTensors, 1),
    F(interpreter_inputs, 1),
    F(interpreter_getInputName, 2),
    F_CPU(interpreter_input_tensor, 3),
    F_CPU(interpreter_invoke, 1),
    F(interpreter_outputs, 1),
    F(interpreter_getOutputName, 2),
    F_CPU(interpreter_output_tensor, 2),
    F(interpreter_tensor, 2),

    F(tflitetensor_type, 1),
    F(tflitetensor_dims, 1),

    F_IO(tflite_printInterpreterState, 1)
};

ERL_NIF_INIT(Elixir.TFLite.Nif, nif_functions, on_load, on_reload, on_upgrade, NULL);
