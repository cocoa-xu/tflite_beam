#ifndef TFLITE_ELIXIR_ERLANG_NIF_RESOURCE_HPP
#define TFLITE_ELIXIR_ERLANG_NIF_RESOURCE_HPP

#pragma once

#include <erl_nif.h>

template<typename R>
struct erlang_nif_res {
    R val;
    int peak;
    static ErlNifResourceType * type;
};
template<typename R> ErlNifResourceType * erlang_nif_res<R>::type = nullptr;

#endif //TFLITE_ELIXIR_ERLANG_NIF_RESOURCE_HPP
