#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.hpp"
#include "../helper.h"
#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/model.h"
#include "tflite_interpreter.h"
#include "tflite_tflitetensor.h"
#include "tflite_status.h"

ERL_NIF_TERM interpreter_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

ERL_NIF_TERM interpreter_allocateTensors(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
                default:
                    return erlang::nif::error(env, "unknown error");
            }
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

ERL_NIF_TERM interpreter_inputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

ERL_NIF_TERM interpreter_getInputName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

ERL_NIF_TERM interpreter_input_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

ERL_NIF_TERM interpreter_invoke(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    erlang_nif_res<tflite::Interpreter *> *self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **) &self_res)) {
        if (self_res->val) {
            return tflite_status_to_erl_term(env, self_res->val->Invoke());
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

ERL_NIF_TERM interpreter_outputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

ERL_NIF_TERM interpreter_getOutputName(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

ERL_NIF_TERM interpreter_output_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

ERL_NIF_TERM interpreter_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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
                tensor_res->peak = 1;

                ERL_NIF_TERM tensor_type;
                if (_tflitetensor_type(env, tensor_res->val, tensor_type)) {
                    tensor_type = enif_make_atom(env, "unknown");
                }

                ERL_NIF_TERM tensor_shape;
                if (_tflitetensor_shape(env, tensor_res->val, tensor_shape)) {
                    return erlang::nif::error(env, "cannot allocate memory for storing tensor shape");
                }

                ERL_NIF_TERM tensor_shape_signature;
                if (_tflitetensor_shape_signature(env, tensor_res->val, tensor_shape_signature)) {
                    return erlang::nif::error(env, "cannot allocate memory for storing tensor shape signature");
                }

                ERL_NIF_TERM tensor_name;
                if (_tflitetensor_name(env, tensor_res->val, tensor_name)) {
                    return erlang::nif::error(env, "cannot allocate memory for storing tensor name");
                }

                ERL_NIF_TERM tensor_quantization_params;
                if (_tflitetensor_quantization_params(env, tensor_res->val, tensor_quantization_params)) {
                    return erlang::nif::error(env, "cannot allocate memory for storing tensor quantization params");
                }

                ERL_NIF_TERM tensor_sparsity_params;
                if (_tflitetensor_sparsity_params(env, tensor_res->val, tensor_sparsity_params)) {
                    return erlang::nif::error(env, "cannot allocate memory for storing tensor sparsity params");
                }

                ERL_NIF_TERM tensor_reference = enif_make_resource(env, tensor_res);
                enif_release_resource(tensor_res);

                return erlang::nif::ok(env, enif_make_tuple8(
                        env,
                        tensor_name,
                        index_nif,
                        tensor_shape,
                        tensor_shape_signature,
                        tensor_type,
                        tensor_quantization_params,
                        tensor_sparsity_params,
                        tensor_reference
                ));
            } else {
                return erlang::nif::error(env, "cannot allocate memory for resource");
            }
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

ERL_NIF_TERM interpreter_setNumThreads(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM num_threads_nif = argv[1];
    int num_threads = -1;
    erlang_nif_res<tflite::Interpreter *> * self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **)&self_res) &&
        erlang::nif::get(env, num_threads_nif, &num_threads)) {
        if (self_res->val) {
            auto status = self_res->val->SetNumThreads(num_threads);
            return tflite_status_to_erl_term(env, status);
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

ERL_NIF_TERM interpreter_get_signature_defs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    erlang_nif_res<tflite::Interpreter *> * self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **)&self_res)) {
        if (self_res->val) {
            auto interpreter_ = self_res->val;
            ERL_NIF_TERM result;

            size_t num_items = interpreter_->signature_keys().size();
            if (num_items == 0) return erlang::nif::ok(env, erlang::nif::atom(env, "nil"));

            ERL_NIF_TERM * keys = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * num_items);
            if (keys == nullptr) {
                return erlang::nif::error(env, "out of memory");
            }
            ERL_NIF_TERM * vals = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * num_items);
            if (vals == nullptr) {
                enif_free(keys);
                return erlang::nif::error(env, "out of memory");
            }

            size_t sig_key_index = 0;
            ERL_NIF_TERM signature_def_keys[2];
            signature_def_keys[0] = erlang::nif::atom(env, "inputs");
            signature_def_keys[1] = erlang::nif::atom(env, "outputs");
            for (const auto& sig_key : interpreter_->signature_keys()) {
                ERL_NIF_TERM signature_def_vals[2];
                const auto& signature_def_inputs =
                        interpreter_->signature_inputs(sig_key->c_str());
                const auto& signature_def_outputs =
                        interpreter_->signature_outputs(sig_key->c_str());

                size_t inputs_items = signature_def_inputs.size();
                ERL_NIF_TERM * inputs_keys = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * inputs_items);
                if (inputs_keys == nullptr) {
                    enif_free(keys);
                    enif_free(vals);
                    return erlang::nif::error(env, "out of memory");
                }
                ERL_NIF_TERM * inputs_vals = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * inputs_items);
                if (inputs_keys == nullptr) {
                    enif_free(keys);
                    enif_free(vals);
                    enif_free(inputs_keys);
                    return erlang::nif::error(env, "out of memory");
                }

                size_t input_item_index = 0;
                for (const auto& input : signature_def_inputs) {
                    inputs_keys[input_item_index] = erlang::nif::atom(env, input.first.c_str());
                    inputs_vals[input_item_index] = erlang::nif::make(env, (long)input.second);
                    input_item_index++;
                }
                enif_make_map_from_arrays(env, inputs_keys, inputs_vals, input_item_index, &signature_def_vals[0]);
                enif_free(inputs_keys);
                enif_free(inputs_vals);

                size_t outputs_items = signature_def_inputs.size();
                ERL_NIF_TERM * outputs_keys = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * outputs_items);
                if (outputs_keys == nullptr) {
                    enif_free(keys);
                    enif_free(vals);
                    return erlang::nif::error(env, "out of memory");
                }
                ERL_NIF_TERM * outputs_vals = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * outputs_items);
                if (outputs_keys == nullptr) {
                    enif_free(keys);
                    enif_free(vals);
                    enif_free(outputs_keys);
                    return erlang::nif::error(env, "out of memory");
                }
                size_t output_item_index = 0;
                for (const auto& output : signature_def_outputs) {
                    outputs_keys[input_item_index] = erlang::nif::atom(env, output.first.c_str());
                    outputs_vals[input_item_index] = erlang::nif::make(env, (long)output.second);
                    output_item_index++;
                }
                enif_make_map_from_arrays(env, outputs_keys, outputs_vals, outputs_items, &signature_def_vals[1]);
                enif_free(outputs_keys);
                enif_free(outputs_vals);

                keys[sig_key_index] = erlang::nif::atom(env, sig_key->c_str());
                enif_make_map_from_arrays(env, signature_def_keys, signature_def_vals, 2, &vals[sig_key_index]);
                sig_key_index++;
            }

            enif_make_map_from_arrays(env, keys, vals, num_items, &result);
            enif_free(keys);
            enif_free(vals);
            return erlang::nif::ok(env, result);
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}
