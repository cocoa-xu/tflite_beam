#include <string>
#include <map>

#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"
#include "../helper.h"

#include "tensorflow/lite/core/api/verifier.h"
#include "tensorflow/lite/model.h"

#include "flatbuffer_model.h"
#include "error_reporter.h"

#ifndef TFLITE_MCU
ERL_NIF_TERM flatbuffer_model_build_from_file(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    std::string filename;
    NifResErrorReporter * error_reporter_res = nullptr;

    ERL_NIF_TERM filename_term = argv[0];
    ERL_NIF_TERM error_reporter_term = argv[1];

    ERL_NIF_TERM ret;

    if (!erlang::nif::get(env, filename_term, filename)) {
        return erlang::nif::error(env, "empty filename");
    }

    tflite::ErrorReporter * error_reporter = nullptr;
    if (!_get_error_reporter(env, error_reporter_term, error_reporter_res, error_reporter, ret)) {
        return ret;
    }

    auto m = tflite::FlatBufferModel::BuildFromFile(filename.c_str(), error_reporter);
    _make_flatbuffer_model_resource(env, m, ret);
    return ret;
}

ERL_NIF_TERM flatbuffer_model_verify_and_build_from_file(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    std::string filename;
    NifResErrorReporter * error_reporter_res = nullptr;

    ERL_NIF_TERM filename_term = argv[0];
    ERL_NIF_TERM error_reporter_term = argv[1];

    ERL_NIF_TERM ret;

    if (!erlang::nif::get(env, filename_term, filename)) {
        return erlang::nif::error(env, "expecting the filename to be a string");
    }

    tflite::TfLiteVerifier * verifier = nullptr;
    tflite::ErrorReporter * error_reporter = nullptr;
    if (!_get_error_reporter(env, error_reporter_term, error_reporter_res, error_reporter, ret)) {
        return ret;
    }

    auto m = tflite::FlatBufferModel::VerifyAndBuildFromFile(filename.c_str(), verifier, error_reporter);
    if (m.get() == nullptr) {
        return erlang::nif::atom(env, "invalid");
    }

    _make_flatbuffer_model_resource(env, m, ret);
    return ret;
}

#else

ERL_NIF_TERM flatbuffer_model_build_from_file(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return erlang::nif::error(env, "FlatBufferModel::BuildFromFile is not available: "
                                   "Library compiled with TFLITE_MCU");
}

ERL_NIF_TERM flatbuffer_model_verify_and_build_from_file(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    return erlang::nif::error(env, "FlatBufferModel::VerifyAndBuildFromFile is not available: "
                                   "Library compiled with TFLITE_MCU");
}

#endif

ERL_NIF_TERM flatbuffer_model_build_from_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ErlNifBinary data;
    NifResErrorReporter * error_reporter_res = nullptr;

    ERL_NIF_TERM data_term = argv[0];
    ERL_NIF_TERM error_reporter_term = argv[1];

    ERL_NIF_TERM ret;

    if (!enif_inspect_binary(env, data_term, &data)) {
        return erlang::nif::error(env, "cannot get input data");
    }

    tflite::ErrorReporter * error_reporter = nullptr;
    if (!_get_error_reporter(env, error_reporter_term, error_reporter_res, error_reporter, ret)) {
        return ret;
    }

    auto m = tflite::FlatBufferModel::BuildFromBuffer((const char *)data.data, data.size, error_reporter);
    _make_flatbuffer_model_resource(env, m, ret);

    return ret;
}

ERL_NIF_TERM flatbuffer_model_initialized(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;

    if (!enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res) || self_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access resource");
    }

    if (self_res->val->initialized()) {
        return erlang::nif::atom(env, "true");
    } else {
        return erlang::nif::atom(env, "false");
    }
}

ERL_NIF_TERM flatbuffer_model_error_reporter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;

    if (!enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res) || self_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access resource");
    }

    auto e = self_res->val->error_reporter();
    if (e == nullptr) {
        return erlang::nif::error(env, "error_reporter is null");   
    }

    ERL_NIF_TERM ret;
    _make_error_reporter(env, e, ret);
    return ret;
}

ERL_NIF_TERM flatbuffer_model_get_minimum_runtime(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;

    if (!enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res) || self_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access resource");
    }

    auto runtime = self_res->val->GetMinimumRuntime();
    return erlang::nif::make_binary(env, runtime.c_str());
}

ERL_NIF_TERM flatbuffer_model_read_all_metadata(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;

    if (!enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res) || self_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access resource");
    }

    std::map<std::string, std::string> metadata = self_res->val->ReadAllMetadata();
    size_t cnt = metadata.size();
    ERL_NIF_TERM ret;
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
        if (iter.first.length() > 0 && iter.second.length() > 0) {
            keys[index] = erlang::nif::make_binary(env, iter.first);
            if (iter.first == "min_runtime_version") {
                const char * data = iter.second.c_str();
                // Get the real length of the runtime string, since there might be
                // trailing
                // '\0's in the buffer.
                bool ok = false;
                for (int len = 0; len < iter.second.size(); ++len) {
                    if (data[len] == '\0') {
                        values[index] = erlang::nif::make_binary(env, std::string(data, len));
                        ok = true;
                        break;
                    }
                }

                if (!ok) {
                    values[index] = erlang::nif::make_binary(env, "min_runtime_version in model metadata is malformed");
                }
            } else {
                values[index] = erlang::nif::make_binary(env, iter.second);
            }
            index++;
        }
    }

    if (!enif_make_map_from_arrays(env, keys, values, index, &ret)) {
        ret = erlang::nif::error(env, "duplicated keys found in metadata");
    }

    enif_free((void *)keys);
    enif_free((void *)values);
    return ret;
}

// ------------------ internal api ------------------

NifResFlatBufferModel * _make_flatbuffer_model_resource(ErlNifEnv *env, std::unique_ptr<tflite::FlatBufferModel>& m, ERL_NIF_TERM &out) {
    NifResFlatBufferModel * res = nullptr;
    if (m.get() == nullptr) {
        out = erlang::nif::error(env, "cannot get flatbuffer model");
        return res;
    }

    if (!(res = NifResFlatBufferModel::allocate_resource(env, out))) {
        m.reset(nullptr);
        return res;
    } 

    // take ownership
    tflite::FlatBufferModel * model = m.release();
    m.reset(nullptr);
    res->val = model;
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_keep_resource(res);
    out = erlang::nif::ok(env, ret);
    return res;
}
