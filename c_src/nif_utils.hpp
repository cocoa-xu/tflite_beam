#ifndef ERLANG_NIF_UTILS_HPP
#define ERLANG_NIF_UTILS_HPP

#pragma once

#include <erl_nif.h>
#include <stdarg.h>
#include <map>
#include <string>
#include <cstring>
#include <vector>
#include "erlang_nif_resource.h"

namespace erlang {
namespace nif {
// Helper for returning `{:error, msg}` from NIF.
ERL_NIF_TERM error(ErlNifEnv *env, const char *msg);

// Helper for returning `{:ok, term}` from NIF.
ERL_NIF_TERM ok(ErlNifEnv *env);

// Helper for returning `:ok` from NIF.
ERL_NIF_TERM ok(ErlNifEnv *env, ERL_NIF_TERM term);

template<typename T>
int make_f64_list_from_c_array(ErlNifEnv *env, size_t count, T *data, ERL_NIF_TERM &out) {
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return 0;
    }

    ERL_NIF_TERM *terms = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * count);
    if (terms == nullptr) {
        return 1;
    }
    for (size_t i = 0; i < count; ++i) {
        terms[i] = enif_make_double(env, (double)(data[i]));
    }
    out = enif_make_list_from_array(env, terms, (unsigned) count);
    enif_free(terms);
    return 0;
}

template<typename T>
int make_i64_list_from_c_array(ErlNifEnv *env, size_t count, T *data, ERL_NIF_TERM &out) {
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return 0;
    }

    ERL_NIF_TERM *terms = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * count);
    if (terms == nullptr) {
        return 1;
    }
    for (size_t i = 0; i < count; ++i) {
        terms[i] = enif_make_int64(env, (int64_t)(data[i]));
    }
    out = enif_make_list_from_array(env, terms, (unsigned) count);
    enif_free(terms);
    return 0;
}

template<typename T>
int make_u64_list_from_c_array(ErlNifEnv *env, size_t count, T *data, ERL_NIF_TERM &out) {
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return 0;
    }

    ERL_NIF_TERM *terms = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * count);
    if (terms == nullptr) {
        return 1;
    }
    for (size_t i = 0; i < count; ++i) {
        terms[i] = enif_make_uint64(env, (uint64_t)(data[i]));
    }
    out = enif_make_list_from_array(env, terms, (unsigned) count);
    enif_free(terms);
    return 0;
}

template<typename T>
int make_i32_list_from_c_array(ErlNifEnv *env, size_t count, T *data, ERL_NIF_TERM &out) {
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return 0;
    }

    ERL_NIF_TERM *terms = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * count);
    if (terms == nullptr) {
        return 1;
    }
    for (size_t i = 0; i < count; ++i) {
        terms[i] = enif_make_int(env, (int32_t)(data[i]));
    }
    out = enif_make_list_from_array(env, terms, (unsigned) count);
    enif_free(terms);
    return 0;
}

template<typename T>
int make_u32_list_from_c_array(ErlNifEnv *env, size_t count, T *data, ERL_NIF_TERM &out) {
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return 0;
    }

    ERL_NIF_TERM *terms = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * count);
    if (terms == nullptr) {
        return 1;
    }
    for (size_t i = 0; i < count; ++i) {
        terms[i] = enif_make_uint(env, (uint32_t)(data[i]));
    }
    out = enif_make_list_from_array(env, terms, (unsigned) count);
    enif_free(terms);
    return 0;
}

// Numeric types

int get(ErlNifEnv *env, ERL_NIF_TERM term, int *var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, unsigned int *var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, int64_t *var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, uint64_t *var);

int get(ErlNifEnv *env, ERL_NIF_TERM term, double *var);

// Standard types

int get(ErlNifEnv *env, ERL_NIF_TERM term, std::string &var);

ERL_NIF_TERM make(ErlNifEnv *env, bool var);

ERL_NIF_TERM make(ErlNifEnv *env, long var);

ERL_NIF_TERM make(ErlNifEnv *env, int var);

ERL_NIF_TERM make(ErlNifEnv *env, double var);

ERL_NIF_TERM make(ErlNifEnv *env, ErlNifBinary var);

ERL_NIF_TERM make(ErlNifEnv *env, std::string var);

ERL_NIF_TERM make(ErlNifEnv *env, const char *string);

int make(ErlNifEnv *env, const std::vector<uint8_t>& array, ERL_NIF_TERM &out);
int make(ErlNifEnv *env, const std::vector<uint16_t>& array, ERL_NIF_TERM &out);
int make(ErlNifEnv *env, const std::vector<uint32_t>& array, ERL_NIF_TERM &out);
int make(ErlNifEnv *env, const std::vector<uint64_t>& array, ERL_NIF_TERM &out);
int make(ErlNifEnv *env, const std::vector<int8_t>& array, ERL_NIF_TERM &out);
int make(ErlNifEnv *env, const std::vector<int16_t>& array, ERL_NIF_TERM &out);
int make(ErlNifEnv *env, const std::vector<int32_t>& array, ERL_NIF_TERM &out);
int make(ErlNifEnv *env, const std::vector<int64_t>& array, ERL_NIF_TERM &out);
int make(ErlNifEnv *env, const std::vector<float>& array, ERL_NIF_TERM &out);
int make(ErlNifEnv *env, const std::vector<double>& array, ERL_NIF_TERM &out);

ERL_NIF_TERM make_binary(ErlNifEnv *env, const char *c_string);

// Atoms

int get_atom(ErlNifEnv *env, ERL_NIF_TERM term, std::string &var);
ERL_NIF_TERM atom(ErlNifEnv *env, const char *msg);

// Check if :nil
int check_nil(ErlNifEnv *env, ERL_NIF_TERM term);

// Boolean

int get(ErlNifEnv *env, ERL_NIF_TERM term, bool *var);

// Containers

int get_tuple(ErlNifEnv *env, ERL_NIF_TERM tuple, std::vector<int64_t> &var);
int get_list(ErlNifEnv *env, ERL_NIF_TERM list, std::vector<ErlNifBinary> &var);
int get_list(ErlNifEnv *env, ERL_NIF_TERM list, std::vector<std::string> &var);
int get_list(ErlNifEnv *env, ERL_NIF_TERM list, std::vector<int> &var);
int get_list(ErlNifEnv *env, ERL_NIF_TERM list, std::vector<int64_t> &var);

}
}

#endif  // ERLANG_NIF_UTILS_HPP
