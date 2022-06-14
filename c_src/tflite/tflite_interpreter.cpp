#include "tflite_interpreter.h"
#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../helper.h"
#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/model.h"
#include "tflite_tflitetensor.h"

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
            return tflite_status_to_erl_term(self_res->val->Invoke(), env);
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
    erlang_nif_res<tflite::InterpreterBuilder *> * self_res;
    if (enif_get_resource(env, self_nif, erlang_nif_res<tflite::Interpreter *>::type, (void **)&self_res) &&
        erlang::nif::get(env, num_threads_nif, &num_threads)) {
        if (self_res->val) {
            auto status = self_res->val->SetNumThreads(num_threads);
            return tflite_status_to_erl_term(status, env);
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}
