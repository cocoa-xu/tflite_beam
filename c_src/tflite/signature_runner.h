#ifndef TFLITE_BEAM_SIGNATURE_RUNNER_H
#define TFLITE_BEAM_SIGNATURE_RUNNER_H

#pragma once

ERL_NIF_TERM interpreter_get_signature_runner(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_signature_key(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_input_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_output_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_input_names(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_output_names(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_input_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_output_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_resize_input_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_resize_input_tensor_strict(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_allocate_tensors(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_invoke(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM signature_runner_cancel(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#endif
