#include <erl_nif.h>
#include <string>
#include <vector>

#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"
#include "../fault_inject.hpp"

#include "signature_runner.h"
#include "status.h"

// The runner belongs to the interpreter that handed it out and lives exactly as long,
// so the resource holds a reference to the interpreter and never deletes the runner.
ERL_NIF_TERM interpreter_get_signature_runner(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM signature_key_nif = argv[1];
    NifResInterpreter * self_res;
    NifResSignatureRunner * res = nullptr;
    std::string signature_key;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    // GetSignatureRunner below builds a placeholder SignatureDef on demand and
    // writes it into the interpreter, so this reads as a lookup and is a
    // mutation. It serialises against invoke and allocate_tensors like any other.
    InterpreterInUse in_use(self_res);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    // nil asks TFLite for the primary subgraph: the first signature pointing at it, or
    // a placeholder one for a model that declares no signatures at all
    bool any_signature = erlang::nif::check_nil(env, signature_key_nif);
    if (!any_signature && !erlang::nif::get(env, signature_key_nif, signature_key)) {
        return erlang::nif::error(env, "expecting `signature_key` to be a string or nil");
    }

    tflite::SignatureRunner * runner =
        self_res->val->GetSignatureRunner(any_signature ? nullptr : signature_key.c_str());
    if (runner == nullptr) {
        return erlang::nif::error(env, "cannot find a signature with the given key");
    }

    if (!(res = NifResSignatureRunner::allocate_resource(env, ret))) {
        return ret;
    }
    ResourceRef<NifResSignatureRunner> hold(res);

    res->val = runner;
    res->interpreter = self_res;
    enif_keep_resource(self_res);

    // Registered so that building into this interpreter again can retire the
    // runner: operator() destroys the tflite::Interpreter this borrows from, and
    // keeping the resource alive does not keep the borrowed pointer valid.
    //
    // The registry holds a bare pointer and takes no reference. Taking one would
    // keep every runner a caller ever asked for alive until the interpreter went
    // away. The runner removes itself in its destructor instead, which is the
    // only moment the pointer could go stale.
    // Growing the registry is the one step here that can fail, and it fails by
    // throwing. Everything it would leave behind is now owned by something that
    // unwinds: the lock by MutexLock, this runner by hold, and the reference it
    // took on the interpreter by its own destructor.
    if (self_res->signature_runners) {
        MutexLock registry(self_res->signature_runners_lock);
        erlang::nif::fault_point(erlang::nif::kFaultRunnerRegistry);
        self_res->signature_runners->push_back(res);
    }

    ret = enif_make_resource(env, res);
    return erlang::nif::ok(env, ret);
}

ERL_NIF_TERM signature_runner_signature_key(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResSignatureRunner * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    return erlang::nif::ok(env, erlang::nif::make_binary(env, self_res->val->signature_key().c_str()));
}

ERL_NIF_TERM signature_runner_input_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResSignatureRunner * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    return erlang::nif::ok(env, enif_make_uint64(env, self_res->val->input_size()));
}

ERL_NIF_TERM signature_runner_output_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResSignatureRunner * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    return erlang::nif::ok(env, enif_make_uint64(env, self_res->val->output_size()));
}

static ERL_NIF_TERM _names_to_list(ErlNifEnv *env, const std::vector<const char *>& names) {
    std::vector<ERL_NIF_TERM> terms;
    terms.reserve(names.size());
    for (auto name : names) {
        terms.push_back(erlang::nif::make_binary(env, name));
    }
    return enif_make_list_from_array(env, terms.data(), (unsigned)terms.size());
}

ERL_NIF_TERM signature_runner_input_names(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResSignatureRunner * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    return erlang::nif::ok(env, _names_to_list(env, self_res->val->input_names()));
}

ERL_NIF_TERM signature_runner_output_names(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResSignatureRunner * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    return erlang::nif::ok(env, _names_to_list(env, self_res->val->output_names()));
}

// Like the interpreter's input_tensor/3, this takes the data rather than handing out a
// tensor whose lifetime the caller would have to reason about.
ERL_NIF_TERM signature_runner_input_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    NifResSignatureRunner * self_res;
    std::string input_name;
    ErlNifBinary data;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    if (!erlang::nif::get(env, argv[1], input_name)) {
        return erlang::nif::error(env, "expecting `input_name` to be a string");
    }

    if (!enif_inspect_binary(env, argv[2], &data)) {
        return erlang::nif::error(env, "cannot get input data");
    }

    TfLiteTensor * t = self_res->val->input_tensor(input_name.c_str());
    if (t == nullptr) {
        return erlang::nif::error(env, "cannot find an input with the given name");
    }

    if (t->data.data == nullptr) {
        return erlang::nif::error(env, "tensor is not allocated yet? Please call allocate_tensors first");
    }

    size_t maximum_bytes = t->bytes;
    if (data.size < maximum_bytes) {
        maximum_bytes = data.size;
    }
    memcpy(t->data.data, data.data, maximum_bytes);
    return erlang::nif::ok(env);
}

ERL_NIF_TERM signature_runner_output_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    NifResSignatureRunner * self_res;
    std::string output_name;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    if (!erlang::nif::get(env, argv[1], output_name)) {
        return erlang::nif::error(env, "expecting `output_name` to be a string");
    }

    const TfLiteTensor * t = self_res->val->output_tensor(output_name.c_str());
    if (t == nullptr) {
        return erlang::nif::error(env, "cannot find an output with the given name");
    }

    if (t->data.data == nullptr) {
        return erlang::nif::error(env, "tensor is not allocated yet? Please call allocate_tensors first");
    }

    ErlNifBinary tensor_data;
    if (!enif_alloc_binary(t->bytes, &tensor_data)) {
        return erlang::nif::error(env, "cannot allocate enough memory for the tensor");
    }

    memcpy(tensor_data.data, t->data.data, t->bytes);
    return erlang::nif::ok(env, enif_make_binary(env, &tensor_data));
}

static ERL_NIF_TERM _resize(ErlNifEnv *env, const ERL_NIF_TERM argv[], bool strict) {
    NifResSignatureRunner * self_res;
    std::string input_name;
    std::vector<int> dims;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    if (!erlang::nif::get(env, argv[1], input_name)) {
        return erlang::nif::error(env, "expecting `input_name` to be a string");
    }

    if (!erlang::nif::get_list(env, argv[2], dims)) {
        return erlang::nif::error(env, "expecting `dims` to be a list of non-negative integers");
    }

    TfLiteStatus status = strict
        ? self_res->val->ResizeInputTensorStrict(input_name.c_str(), dims)
        : self_res->val->ResizeInputTensor(input_name.c_str(), dims);
    return tflite_status_to_erl_term(env, status);
}

ERL_NIF_TERM signature_runner_resize_input_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);
    return _resize(env, argv, false);
}

ERL_NIF_TERM signature_runner_resize_input_tensor_strict(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);
    return _resize(env, argv, true);
}

ERL_NIF_TERM signature_runner_allocate_tensors(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResSignatureRunner * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    return tflite_status_to_erl_term(env, self_res->val->AllocateTensors());
}

ERL_NIF_TERM signature_runner_invoke(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResSignatureRunner * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    return tflite_status_to_erl_term(env, self_res->val->Invoke());
}

ERL_NIF_TERM signature_runner_cancel(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResSignatureRunner * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResSignatureRunner::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // A runner works on a subgraph the interpreter owns, so it takes that
    // interpreter's guard rather than one of its own: a lock private to the
    // runner would serialise it against itself and against nothing else.
    InterpreterInUse in_use(self_res->interpreter);
    if (!in_use.acquired()) {
        return erlang::nif::error(env, "interpreter is already in use by another process");
    }

    return tflite_status_to_erl_term(env, self_res->val->Cancel());
}
