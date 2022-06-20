#ifndef TFLITE_HELPER_H
#define TFLITE_HELPER_H

#pragma once

#include <erl_nif.h>
#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"
#include "erlang_nif_resource.hpp"

bool tensor_type_to_erl_term(const TfLiteType in_type, ErlNifEnv *env, ERL_NIF_TERM &out_term);
bool tensor_type_from_erl_term(ErlNifEnv *env, const ERL_NIF_TERM in_term, TfLiteType &out_type);
ERL_NIF_TERM tflite_status_to_erl_term(const TfLiteStatus status, ErlNifEnv *env);

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

#endif  // TFLITE_HELPER_H
