#include <string>
#include <map>

#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.hpp"
#include "../helper.h"

#include "tensorflow/lite/model.h"

#include "flatbuffer_model.h"

#ifndef TFLITE_MCU
ERL_NIF_TERM flatBufferModel_buildFromFile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    std::string filename;
    if (erlang::nif::get(env, argv[0], filename)) {
        NifResFlatBufferModel * res;
        auto m = tflite::FlatBufferModel::BuildFromFile(filename.c_str());
        if (m.get() != nullptr) {
            if (alloc_resource(&res)) {
                // take ownership
                tflite::FlatBufferModel * model = m.release();
                res->val = model;
                ERL_NIF_TERM ret = enif_make_resource(env, res);
                enif_release_resource(res);
                return erlang::nif::ok(env, ret);
            } else {
                // free
                m.reset(nullptr);
                return erlang::nif::error(env, "cannot allocate memory for resource");
            }
        } else {
            return erlang::nif::error(env, "cannot load flat buffer model from file");
        }
    } else {
        return erlang::nif::error(env, "empty filename");
    }
}
#else
ERL_NIF_TERM flatBufferModel_buildFromFile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return erlang::nif::error(env, "FlatBufferModel::BuildFromFile is not available: "
                                   "Library compiled with TFLITE_MCU");
}
#endif

ERL_NIF_TERM flatBufferModel_buildFromBuffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM buffer_nif = argv[0];
    ErlNifBinary data;
    if (enif_inspect_binary(env, buffer_nif, &data)) {
        NifResFlatBufferModel * res;
        auto m = tflite::FlatBufferModel::BuildFromBuffer((const char *)data.data, data.size);
        if (m.get() != nullptr) {
            if (alloc_resource(&res)) {
                // take ownership
                tflite::FlatBufferModel * model = m.release();
                res->val = model;
                ERL_NIF_TERM ret = enif_make_resource(env, res);
                enif_release_resource(res);
                return erlang::nif::ok(env, ret);
            } else {
                // free
                m.reset(nullptr);
                return erlang::nif::error(env, "cannot allocate memory for resource");
            }
        } else {
            return erlang::nif::error(env, "cannot load flat buffer model from file");
        }
    } else {
        return erlang::nif::error(env, "cannot get input data");
    }
}

ERL_NIF_TERM flatBufferModel_initialized(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;
    if (enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res)) {
        if (self_res->val) {
            if (self_res->val->initialized()) {
                return erlang::nif::atom(env, "true");
            } else {
                return erlang::nif::atom(env, "false");
            }
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

ERL_NIF_TERM flatBufferModel_getMinimumRuntime(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;
    if (enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res)) {
        if (self_res->val) {
            auto runtime = self_res->val->GetMinimumRuntime();
            return erlang::nif::make_binary(env, runtime.c_str());
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}

ERL_NIF_TERM flatBufferModel_readAllMetadata(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;
    if (enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res)) {
        if (self_res->val) {
            auto metadata = self_res->val->ReadAllMetadata();
            size_t cnt = metadata.size();
            ERL_NIF_TERM ret;
            ERL_NIF_TERM map_out;
            ERL_NIF_TERM * keys = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * cnt);
            ERL_NIF_TERM * values = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * cnt);
            size_t index = 0;
            for (auto &iter : metadata) {
                keys[index] = erlang::nif::make_binary(env, iter.first.c_str());
                values[index] = erlang::nif::make_binary(env, iter.second.c_str());
                index++;
            }
            // enif_make_map_from_arrays returns false
            // if there are any duplicate keys. But it is practically impossible
            // here.
            if (!enif_make_map_from_arrays(env, keys, values, index, &map_out)) {
                ret = erlang::nif::error(env, "oh nyo erlang");
            } else {
                ret = map_out;
            }
            enif_free((void *)keys);
            enif_free((void *)values);
            return ret;
        } else {
            return erlang::nif::error(env, "oh nyo erlang");
        }
    } else {
        return erlang::nif::error(env, "cannot access resource");
    }
}
