#ifndef TFLITE_HELPER_H
#define TFLITE_HELPER_H

#pragma once

#include <erl_nif.h>
#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"
#include "erlang_nif_resource.hpp"

bool tensor_type_to_erl_term(const TfLiteType in_type, ErlNifEnv *env, ERL_NIF_TERM &out_term);
bool tensor_type_from_erl_term(ErlNifEnv *env, const ERL_NIF_TERM in_term, TfLiteType &out_type);

NifResBuiltinOpResolver * alloc_resource_NifResBuiltinOpResolver();
NifResInterpreterBuilder * alloc_resource_NifResInterpreterBuilder();
NifResFlatBufferModel * alloc_resource_NifResFlatBufferModel();
NifResInterpreter * alloc_resource_NifResInterpreter();
NifResErrorReporter * alloc_resource_NifResErrorReporter();
NifResTfLiteTensor * alloc_resource_NifResTfLiteTensor();

#ifdef CORAL_SUPPORT_ENABLED
NifResEdgeTpuContext * alloc_resource_NifResEdgeTpuContext();
#endif

static void destruct_edge_tpu_context(ErlNifEnv *env, void *args) {
    auto res = (NifResEdgeTpuContext *)args;
    if (res->val) {
        delete res->val;
        res->val = nullptr;
    }
}

static void destruct_builtin_op_resolver(ErlNifEnv *env, void *args) {
    auto res = (NifResBuiltinOpResolver *)args;
    if (res) {
        if (res->val) {
            res->dropped_in_erlang = true;
            if (!res->deleted && res->reference_count == 0) {
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
            if (!res->deleted && res->reference_count == 0) {
                delete res->val;
                res->val = nullptr;
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
                    res->op_resolver->deleted = true;
                    res->op_resolver = nullptr;
                }
            }

            if (res->flatbuffer_model) {
                res->flatbuffer_model->reference_count--;

                if (res->flatbuffer_model->reference_count == 0 && res->flatbuffer_model->dropped_in_erlang) {
                    if (res->flatbuffer_model->val) {
                        delete res->flatbuffer_model->val;
                    }
                    res->flatbuffer_model->val = nullptr;
                    res->flatbuffer_model->deleted = true;
                    enif_release_resource(res->flatbuffer_model);
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
                res->flatbuffer_model->reference_count--;

                if (res->flatbuffer_model->reference_count == 0 && res->flatbuffer_model->dropped_in_erlang) {
                    if (res->flatbuffer_model->val) {
                        delete res->flatbuffer_model->val;
                    }
                    res->flatbuffer_model->val = nullptr;
                    res->flatbuffer_model->deleted = true;
                    enif_release_resource(res->flatbuffer_model);
                    res->flatbuffer_model = nullptr;
                }
            }
            delete res->val;
            res->val = nullptr;
        }
    }
}

static void destruct_error_reporter(ErlNifEnv *env, void *args) {
    auto res = (NifResErrorReporter *)args;
    if (res) {
        if (res->val) {
            if (!res->is_default) {
                delete res->val;
            }
            res->val = nullptr;
        }
    }
}

static void destruct_tensor_ptr(ErlNifEnv *env, void *args) {
    auto res = (NifResTfLiteTensor *)args;
    if (res) {
        if (res->val) {
            if (!res->borrowed) {
                delete res->val;
            }
            res->val = nullptr;
        }
    }
}

#endif  // TFLITE_HELPER_H
