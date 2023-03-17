#include <string>
#include <map>

#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.hpp"
#include "../helper.h"

#include "tensorflow/lite/core/api/verifier.h"
#include "tensorflow/lite/model.h"

#include "flatbuffer_model.h"
#include "error_reporter.h"

#ifndef TFLITE_MCU
ERL_NIF_TERM flatBufferModel_buildFromFile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    std::string filename;
    NifResErrorReporter * error_reporter_res = nullptr;

    ERL_NIF_TERM filename_term = argv[0];
    ERL_NIF_TERM error_reporter_term = argv[1];

    ERL_NIF_TERM ret;

    if (erlang::nif::get(env, filename_term, filename)) {
        tflite::ErrorReporter * error_reporter = nullptr;
        if (!_get_error_reporter(env, error_reporter_term, error_reporter_res, error_reporter, ret)) {
            return ret;
        }

        auto m = tflite::FlatBufferModel::BuildFromFile(filename.c_str(), error_reporter);
        _make_flatbuffer_model_resource(env, m, ret);
        return ret;
    } else {
        return erlang::nif::error(env, "empty filename");
    }
}

ERL_NIF_TERM flatBufferModel_verifyAndBuildFromFile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    std::string filename;
    NifResErrorReporter * error_reporter_res = nullptr;

    ERL_NIF_TERM filename_term = argv[0];
    ERL_NIF_TERM error_reporter_term = argv[1];

    ERL_NIF_TERM ret;

    if (erlang::nif::get(env, filename_term, filename)) {
        tflite::TfLiteVerifier * verifier = nullptr;
        tflite::ErrorReporter * error_reporter = nullptr;
        if (!_get_error_reporter(env, error_reporter_term, error_reporter_res, error_reporter, ret)) {
            return ret;
        }

        auto m = tflite::FlatBufferModel::VerifyAndBuildFromFile(filename.c_str(), verifier, error_reporter);
        if (m.get() == nullptr) {
            ret = erlang::nif::atom(env, "invalid");
        } else {
            _make_flatbuffer_model_resource(env, m, ret);
        }
        return ret;
    } else {
        return erlang::nif::error(env, "empty filename");
    }
}
#else
ERL_NIF_TERM flatBufferModel_buildFromFile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return erlang::nif::error(env, "FlatBufferModel::BuildFromFile is not available: "
                                   "Library compiled with TFLITE_MCU");
}

ERL_NIF_TERM flatBufferModel_VerifyAndBuildFromFile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return erlang::nif::error(env, "FlatBufferModel::VerifyAndBuildFromFile is not available: "
                                   "Library compiled with TFLITE_MCU");
}
#endif

ERL_NIF_TERM flatBufferModel_buildFromBuffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ErlNifBinary data;
    NifResErrorReporter * error_reporter_res = nullptr;

    ERL_NIF_TERM data_term = argv[0];
    ERL_NIF_TERM error_reporter_term = argv[1];

    ERL_NIF_TERM ret;

    if (enif_inspect_binary(env, data_term, &data)) {
        tflite::ErrorReporter * error_reporter = nullptr;
        if (!_get_error_reporter(env, error_reporter_term, error_reporter_res, error_reporter, ret)) {
            return ret;
        }

        auto m = tflite::FlatBufferModel::BuildFromBuffer((const char *)data.data, data.size, error_reporter);
        _make_flatbuffer_model_resource(env, m, ret);

        return ret;
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

ERL_NIF_TERM flatBufferModel_error_reporter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;

    ERL_NIF_TERM ret;

    if (enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res)) {
        if (self_res->val) {
            auto e = self_res->val->error_reporter();
            if (e) {
                _make_error_reporter(env, e, ret);
                return ret;
            } else {
                return erlang::nif::error(env, "error_reporter is null");
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
            if (!keys) {
                return erlang::nif::error(env, "enif_alloc failed");
            }

            ERL_NIF_TERM * values = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * cnt);
            if (!values) {
                enif_free((void *)keys);
                return erlang::nif::error(env, "enif_alloc failed");
            }

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

// ------------------ internal api ------------------

NifResFlatBufferModel * _make_flatbuffer_model_resource(ErlNifEnv *env, std::unique_ptr<tflite::FlatBufferModel>& m, ERL_NIF_TERM &out) {
    NifResFlatBufferModel * res = nullptr;
    if (m.get() != nullptr) {
        if ((res = alloc_resource_NifResFlatBufferModel())) {
            // take ownership
            tflite::FlatBufferModel * model = m.release();
            m.reset(nullptr);
            res->val = model;
            ERL_NIF_TERM ret = enif_make_resource(env, res);
            enif_keep_resource(res);
            out = erlang::nif::ok(env, ret);
        } else {
            // free
            m.reset(nullptr);
            out = erlang::nif::error(env, "cannot allocate memory for resource");
        }
    } else {
        out = erlang::nif::error(env, "cannot get flatbuffer model");
    }
    return res;
}
