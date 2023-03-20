#ifndef TFLITE_FLATBUFFERMODEL_BINDINGS_H
#define TFLITE_FLATBUFFERMODEL_BINDINGS_H

#pragma once

// --------------------- nif api --------------------

ERL_NIF_TERM flatbuffer_model_build_from_file(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM flatbuffer_model_verify_and_build_from_file(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM flatbuffer_model_build_from_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM flatbuffer_model_initialized(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM flatbuffer_model_error_reporter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM flatbuffer_model_get_minimum_runtime(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM flatbuffer_model_read_all_metadata(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

// ------------------ internal api ------------------

/// @brief Make flatbuffer model nif resource
/// @param env Nif env
/// @param m [in] reference to FlatBufferModel, model ownership will be transferred to res
/// @param out out nif resource term
/// @return out nif resource
NifResFlatBufferModel * _make_flatbuffer_model_resource(ErlNifEnv *env, std::unique_ptr<tflite::FlatBufferModel>& m, ERL_NIF_TERM &out);

#endif // TFLITE_FLATBUFFERMODEL_BINDINGS_H
