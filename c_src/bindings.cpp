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
    static ErlNifResourceType * type;
};
template<typename R> ErlNifResourceType * erlang_nif_res<R>::type = nullptr;

template<typename R>
int alloc_resource(erlang_nif_res<R> **res) {
    *res = (erlang_nif_res<R> *)enif_alloc_resource(erlang_nif_res<R>::type, sizeof(erlang_nif_res<R>));
    return (*res != nullptr);
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

static ERL_NIF_TERM interpreter_inputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    erlang_nif_res<tflite::Interpreter *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **) &self_res)) {
        if (self_res->val) {
            auto inputs = self_res->val->inputs();
            size_t cnt = inputs.size();
            ERL_NIF_TERM * arr = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * cnt);
            for (size_t i = 0; i < cnt; i++) {
                arr[i] = enif_make_int(env, inputs[i]);
            }
            ERL_NIF_TERM ret = enif_make_list_from_array(env, arr, (unsigned)cnt);
            enif_free((void *)arr);
            return erlang::nif::ok(env, ret);
        }
        else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

static ERL_NIF_TERM interpreter_getInputName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM index_nif = argv[1];
    int index;
    erlang_nif_res<tflite::Interpreter *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **) &self_res) &&
        enif_get_int(env, index_nif, &index)) {
        if (self_res->val) {
            auto name = self_res->val->GetInputName(index);
            return erlang::nif::ok(env, erlang::nif::make_binary(env, name));
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

static ERL_NIF_TERM interpreter_input_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM index_nif = argv[1];
    ERL_NIF_TERM data_nif = argv[2];
    int index;
    ErlNifBinary data;
    erlang_nif_res<tflite::Interpreter *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **)&self_res) &&
        enif_get_int(env, index_nif, &index)) {
        if (self_res->val) {
            if (enif_inspect_binary(env, data_nif, &data)) {
                auto input_tensor = self_res->val->input_tensor(index);
                if (input_tensor->data.data == nullptr) {
                    return erlang::nif::error(env, "tensor is not allocated yet? Please call TFLite.Interpreter.allocateTensors first");
                } else {
                    memcpy(input_tensor->data.data, data.data, data.size);
                    return erlang::nif::ok(env);
                }
            } else {
                return erlang::nif::error(env, "cannot get input data");
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

static ERL_NIF_TERM interpreter_outputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    erlang_nif_res<tflite::Interpreter *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **) &self_res)) {
        if (self_res->val) {
            auto outputs = self_res->val->outputs();
            size_t cnt = outputs.size();
            ERL_NIF_TERM * arr = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * cnt);
            for (size_t i = 0; i < cnt; i++) {
                arr[i] = enif_make_int(env, outputs[i]);
            }
            ERL_NIF_TERM ret = enif_make_list_from_array(env, arr, (unsigned)cnt);
            enif_free((void *)arr);
            return erlang::nif::ok(env, ret);
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

static ERL_NIF_TERM interpreter_getOutputName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM index_nif = argv[1];
    int index;
    erlang_nif_res<tflite::Interpreter *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **) &self_res) &&
        enif_get_int(env, index_nif, &index)) {
        if (self_res->val) {
            auto name = self_res->val->GetOutputName(index);
            return erlang::nif::ok(env, erlang::nif::make_binary(env, name));
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

static ERL_NIF_TERM interpreter_output_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM index_nif = argv[1];
    int index;
    erlang_nif_res<tflite::Interpreter *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **) &self_res) &&
        enif_get_int(env, index_nif, &index)) {
        if (self_res->val) {
            auto t = self_res->val->output_tensor(index);
            ErlNifBinary tensor_data;
            size_t tensor_size = t->bytes;
            if (!enif_alloc_binary(tensor_size, &tensor_data))
                return erlang::nif::error(env, "cannot allocate enough memory for the tensor");

            memcpy(tensor_data.data, t->data.raw, tensor_size);
            return erlang::nif::ok(env, enif_make_binary(env, &tensor_data));
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

static ERL_NIF_TERM interpreter_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM index_nif = argv[1];
    int index;
    erlang_nif_res<tflite::Interpreter *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **) &self_res) &&
        enif_get_int(env, index_nif, &index)) {
        if (self_res->val) {
            erlang_nif_res<TfLiteTensor *> * tensor_res;
            if (alloc_resource(&tensor_res)) {
                tensor_res->val = self_res->val->tensor(index);
                ERL_NIF_TERM ret = enif_make_resource(env, tensor_res);
                enif_release_resource(tensor_res);
                return erlang::nif::ok(env, ret);
            } else {
                return erlang::nif::error(env, "cannot allocate memory for resource");
            }
        }
        else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

//
static ERL_NIF_TERM tflitetensor_type(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    erlang_nif_res<TfLiteTensor *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<TfLiteTensor *>::type, (void **) &self_res)) {
        if (self_res->val) {
            ERL_NIF_TERM ret = erlang::nif::error(env, "invalid tensor");
            tensor_type_to_erl_term(self_res->val->type, env, ret);
            return ret;
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

static ERL_NIF_TERM tflitetensor_dims(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    erlang_nif_res<TfLiteTensor *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<TfLiteTensor *>::type, (void **) &self_res)) {
        if (self_res->val) {
            auto dims = self_res->val->dims;
            ERL_NIF_TERM * arr = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * dims->size);
            for (int i = 0; i < dims->size; ++i) {
                arr[i] = enif_make_int(env, dims->data[i]);
            }
            ERL_NIF_TERM ret = enif_make_list_from_array(env, arr, (unsigned)dims->size);
            enif_free((void *)arr);
            return erlang::nif::ok(env, ret);
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
    fprintf(stderr, "release: %s\r\n", __PRETTY_FUNCTION__ );
}

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
