#ifndef TFLITE_TFLITETENSOR_BINDINGS_H
#define TFLITE_TFLITETENSOR_BINDINGS_H

#pragma once

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

#endif // TFLITE_TFLITETENSOR_BINDINGS_H
