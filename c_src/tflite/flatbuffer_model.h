#ifndef TFLITE_FLATBUFFERMODEL_BINDINGS_H
#define TFLITE_FLATBUFFERMODEL_BINDINGS_H

#pragma once

using NifResFlatBufferModel = erlang_nif_res<tflite::FlatBufferModel *>;

ERL_NIF_TERM flatBufferModel_buildFromFile(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM flatBufferModel_buildFromBuffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM flatBufferModel_initialized(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM flatBufferModel_getMinimumRuntime(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM flatBufferModel_readAllMetadata(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif // TFLITE_FLATBUFFERMODEL_BINDINGS_H
