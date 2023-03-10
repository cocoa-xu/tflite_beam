#include "helper.h"
#include <erl_nif.h>
#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"
#include "nif_utils.hpp"

bool tensor_type_to_erl_term(const TfLiteType in_type, ErlNifEnv *env, ERL_NIF_TERM &out_term) {
    bool ok = true;
    switch (in_type) {
        case kTfLiteNoType:
            out_term = erlang::nif::atom(env, "no_type");
            break;
        case kTfLiteFloat32:
            out_term = enif_make_tuple2(env,
                                        enif_make_atom(env, "f"),
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

bool tensor_type_from_erl_term(ErlNifEnv *env, const ERL_NIF_TERM in_term, TfLiteType &out_type) {
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

