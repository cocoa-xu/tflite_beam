#ifndef TFLITE_INTERPRETER_BINDINGS_H
#define TFLITE_INTERPRETER_BINDINGS_H

#pragma once

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
                tensor_res->peak = 1;
                ERL_NIF_TERM ret = enif_make_resource(env, tensor_res);
                enif_release_resource(tensor_res);
                return erlang::nif::ok(env, ret);
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

#endif // TFLITE_INTERPRETER_BINDINGS_H
