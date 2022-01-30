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

#define TFLITE_MINIMAL_CHECK(x)                              \
  if (!(x)) {                                                \
    fprintf(stderr, "Error at %s:%d\n", __FILE__, __LINE__); \
    exit(1);                                                 \
  }

template<typename R>
struct erlang_nif_res {
    R val;
    static ErlNifResourceType * type;
};
template<typename R> ErlNifResourceType * erlang_nif_res<R>::type = nullptr;

template<typename R>
int alloc_resource(erlang_nif_res<R> **res) {
    *res = (erlang_nif_res<R> *)enif_alloc_resource(erlang_nif_res<R>::type, sizeof(erlang_nif_res<R>));
    return (*res != nullptr);
}

// tflite::FlatBufferModel
static ERL_NIF_TERM flatBufferModel_buildFromFile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    std::string filename;
    if (erlang::nif::get(env, argv[0], filename)) {
        erlang_nif_res<tflite::FlatBufferModel *> * res;
        auto m = tflite::FlatBufferModel::BuildFromFile(filename.c_str());
        tflite::FlatBufferModel * model = m.release();
        if (model != nullptr) {
            if (alloc_resource(&res)) {
                res->val = model;
                ERL_NIF_TERM ret = enif_make_resource(env, res);
                enif_release_resource(res);
                return erlang::nif::ok(env, ret);
            } else {
                return erlang::nif::error(env, "cannot allocate memory for resource");
            }
        } else {
            return erlang::nif::error(env, "cannot load flat buffer model from file");
        }
    } else {
        return erlang::nif::error(env, "empty filename");
    }
}

//
static ERL_NIF_TERM ops_builtin_builtinResolver_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    erlang_nif_res<tflite::ops::builtin::BuiltinOpResolver *> * res;
    if (alloc_resource(&res)) {
        res->val = new tflite::ops::builtin::BuiltinOpResolver();
        ERL_NIF_TERM ret = enif_make_resource(env, res);
        enif_release_resource(res);
        return erlang::nif::ok(env, ret);
    } else {
        return erlang::nif::error(env, "cannot allocate memory for resource");
    }
}

//
static ERL_NIF_TERM interpreterBuilder_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM model_nif = argv[0];
    ERL_NIF_TERM resolver_nif = argv[1];
    erlang_nif_res<tflite::FlatBufferModel *> * model_res;
    erlang_nif_res<tflite::ops::builtin::BuiltinOpResolver *> * resolver_res;
    erlang_nif_res<tflite::InterpreterBuilder *> * res;
    if (enif_get_resource(env, model_nif, erlang_nif_res<tflite::FlatBufferModel *>::type, (void **)&model_res) &&
        enif_get_resource(env, resolver_nif, erlang_nif_res<tflite::ops::builtin::BuiltinOpResolver *>::type, (void **)&resolver_res) &&
        alloc_resource(&res)) {
        if (model_res->val && resolver_res->val) {
            res->val = new tflite::InterpreterBuilder(*model_res->val, *resolver_res->val);
            ERL_NIF_TERM ret = enif_make_resource(env, res);
            enif_release_resource(res);
            return erlang::nif::ok(env, ret);
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

static ERL_NIF_TERM interpreterBuilder_build(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM interpreter_nif = argv[1];
    erlang_nif_res<tflite::InterpreterBuilder *> * self_res;
    erlang_nif_res<tflite::Interpreter *> * interpreter_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::InterpreterBuilder *>::type, (void **)&self_res) &&
        enif_get_resource(env, interpreter_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **)&interpreter_res)) {
        if (self_res->val && interpreter_res->val) {
            auto &builder = *self_res->val;
            std::unique_ptr<tflite::Interpreter> pretend(interpreter_res->val);
            builder.operator()(&pretend);
            interpreter_res->val = pretend.release();
            return erlang::nif::ok(env);
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

//
static ERL_NIF_TERM interpreter_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    erlang_nif_res<tflite::Interpreter *> * res;
    if (alloc_resource(&res)) {
        res->val = new tflite::Interpreter();
        ERL_NIF_TERM ret = enif_make_resource(env, res);
        enif_release_resource(res);
        return erlang::nif::ok(env, ret);
    } else {
        return erlang::nif::error(env, "cannot allocate memory for resource");
    }
}

static ERL_NIF_TERM interpreter_allocateTensors(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    erlang_nif_res<tflite::Interpreter *> * self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **)&self_res)) {
        if (self_res->val) {
            switch (self_res->val->AllocateTensors()) {
                case kTfLiteOk:
                    return erlang::nif::atom(env, "ok");
                case kTfLiteError:
                    return erlang::nif::error(env, "General runtime error");
                case kTfLiteDelegateError:
                    return erlang::nif::error(env, "TfLiteDelegate");
                case kTfLiteApplicationError:
                    return erlang::nif::error(env, "Application");
                case kTfLiteDelegateDataNotFound:
                    return erlang::nif::error(env, "DelegateDataNotFound");
                case kTfLiteDelegateDataWriteError:
                    return erlang::nif::error(env, "DelegateDataWriteError");
                case kTfLiteDelegateDataReadError:
                    return erlang::nif::error(env, "DelegateDataReadError");
            }
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

static ERL_NIF_TERM interpreter_invoke(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    erlang_nif_res<tflite::Interpreter *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **) &self_res)) {
        if (self_res->val) {
            switch (self_res->val->Invoke()) {
                case kTfLiteOk:
                    return erlang::nif::atom(env, "ok");
                case kTfLiteError:
                    return erlang::nif::error(env, "General runtime error");
                case kTfLiteDelegateError:
                    return erlang::nif::error(env, "TfLiteDelegate");
                case kTfLiteApplicationError:
                    return erlang::nif::error(env, "Application");
                case kTfLiteDelegateDataNotFound:
                    return erlang::nif::error(env, "DelegateDataNotFound");
                case kTfLiteDelegateDataWriteError:
                    return erlang::nif::error(env, "DelegateDataWriteError");
                case kTfLiteDelegateDataReadError:
                    return erlang::nif::error(env, "DelegateDataReadError");
            }
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

//
static ERL_NIF_TERM tflite_printInterpreterState(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM interpreter_nif = argv[0];
    erlang_nif_res<tflite::Interpreter *> * interpreter_res;
    if (enif_get_resource(env, interpreter_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **)&interpreter_res)) {
        if (interpreter_res->val) {
            tflite::PrintInterpreterState(interpreter_res->val);
            return erlang::nif::atom(env, "nil");
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

template <typename T>
static void destruct_raw_ptr(ErlNifEnv *env, void *args) {
    auto res = (erlang_nif_res<T *> *)args;
    if (res->val) {
        delete res->val;
        res->val = nullptr;
    }
}

static int
on_load(ErlNifEnv* env, void**, ERL_NIF_TERM)
{
    ErlNifResourceType *rt;
    rt = enif_open_resource_type(env, "Elixir.TFLite.Nif", "FlatBufferModel", destruct_raw_ptr<tflite::FlatBufferModel *>, ERL_NIF_RT_CREATE, NULL);                                                             \
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

    F(ops_builtin_builtinResolver_new, 0),

    F(interpreterBuilder_new, 2),
    F(interpreterBuilder_build, 2),

    F(interpreter_new, 0),
    F(interpreter_allocateTensors, 1),
    F_CPU(interpreter_invoke, 1),

    F_IO(tflite_printInterpreterState, 1)
};

ERL_NIF_INIT(Elixir.TFLite.Nif, nif_functions, on_load, on_reload, on_upgrade, NULL);
