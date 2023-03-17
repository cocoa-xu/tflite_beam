#ifndef TFLITE_HELPER_H
#define TFLITE_HELPER_H

#pragma once

#include <erl_nif.h>
#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"
#include "erlang_nif_resource.hpp"

bool tensor_type_to_erl_term(const TfLiteType in_type, ErlNifEnv *env, ERL_NIF_TERM &out_term);
bool tensor_type_from_erl_term(ErlNifEnv *env, const ERL_NIF_TERM in_term, TfLiteType &out_type);

template<typename R>
int alloc_resource(erlang_nif_res<R> **res) {
    *res = (erlang_nif_res<R> *)enif_alloc_resource(erlang_nif_res<R>::type, sizeof(erlang_nif_res<R>));
    return (*res != nullptr);
}

int alloc_resource_NifResBuiltinOpResolver(NifResBuiltinOpResolver **res);
int alloc_resource_NifResInterpreterBuilder(NifResInterpreterBuilder **res);
int alloc_resource_NifResFlatBufferModel(NifResFlatBufferModel **res);
int alloc_resource_NifResInterpreter(NifResInterpreter **res);

template <typename T>
static void destruct_raw_ptr(ErlNifEnv *env, void *args) {
    auto res = (erlang_nif_res<T *> *)args;
    if (res->val && !res->peak) {
        delete res->val;
        res->val = nullptr;
    }
}

static void destruct_builtin_op_resolver(ErlNifEnv *env, void *args) {
    auto res = (NifResBuiltinOpResolver *)args;
    if (res) {
        if (res->val) {
            res->dropped_in_erlang = true;
            if (res->reference_count == 0) {
                delete res->val;
                res->val = nullptr;
            }
        }
    }
}

static void destruct_flatbuffer_model(ErlNifEnv *env, void *args) {
    auto res = (NifResFlatBufferModel *)args;
    if (res) {
        if (res->val) {
            res->dropped_in_erlang = true;
            if (res->reference_count == 0) {
                // delete res->val;
                // res->val = nullptr;
            }
        }
    }
}

static void destruct_interpreter_builder(ErlNifEnv *env, void *args) {
    auto res = (NifResInterpreterBuilder *)args;
    if (res) {
        if (res->val) {
            if (res->op_resolver) {
                res->op_resolver->reference_count--;

                if (res->op_resolver->reference_count == 0 && res->op_resolver->dropped_in_erlang) {
                    if (res->op_resolver->val) {
                        delete res->op_resolver->val;
                    }
                    res->op_resolver->val = nullptr;
                    res->op_resolver = nullptr;
                }
            }

            if (res->flatbuffer_model) {
                printf("drop interpreter_builder: model=%ld, dropped_in_erlang=%d\r\n", res->flatbuffer_model->reference_count.load(), res->flatbuffer_model->dropped_in_erlang.load());
                res->flatbuffer_model->reference_count--;
                printf("drop interpreter_builder: model=%ld, dropped_in_erlang=%d\r\n", res->flatbuffer_model->reference_count.load(), res->flatbuffer_model->dropped_in_erlang.load());

                if (res->flatbuffer_model->reference_count == 0 && res->flatbuffer_model->dropped_in_erlang) {
                    if (res->flatbuffer_model->val) {
                        // delete res->flatbuffer_model->val;
                    }
                    res->flatbuffer_model->val = nullptr;
                    res->flatbuffer_model = nullptr;
                }
            }

            delete res->val;
            res->val = nullptr;
        }
    }
}

static void destruct_interpreter(ErlNifEnv *env, void *args) {
    auto res = (NifResInterpreter *)args;
    if (res) {
        if (res->val) {
            if (res->flatbuffer_model) {
                printf("drop interpreter: model=%ld, dropped_in_erlang=%d\r\n", res->flatbuffer_model->reference_count.load(), res->flatbuffer_model->dropped_in_erlang.load());
                res->flatbuffer_model->reference_count--;
                printf("drop interpreter: model=%ld, dropped_in_erlang=%d\r\n", res->flatbuffer_model->reference_count.load(), res->flatbuffer_model->dropped_in_erlang.load());

                if (res->flatbuffer_model->reference_count == 0 && res->flatbuffer_model->dropped_in_erlang) {
                    if (res->flatbuffer_model->val) {
                        // delete res->flatbuffer_model->val;
                    }
                    res->flatbuffer_model->val = nullptr;
                    res->flatbuffer_model = nullptr;
                }
            }
            delete res->val;
            res->val = nullptr;
        }
    }
}

static void destruct_tensor_ptr(ErlNifEnv *env, void *args) {
    auto res = (erlang_nif_res<TfLiteTensor *> *)args;
    if (res) {
        if (res->val && !res->peak) {
            delete res->val;
            res->val = nullptr;
        }
    }
}

#endif  // TFLITE_HELPER_H
