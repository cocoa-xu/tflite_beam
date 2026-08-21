#include <map>
#include <string>

#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"
#include "../helper.h"

#include "tensorflow/lite/interpreter.h"
#include "tensorflow/lite/kernels/register.h"
#include "tensorflow/lite/model.h"

#include "interpreter.h"
#include "tflitetensor.h"
#include "status.h"

ERL_NIF_TERM interpreter_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    NifResInterpreter * res = nullptr;
    ERL_NIF_TERM ret;

    if (!(res = NifResInterpreter::allocate_resource(env, ret))) {
        return ret;
    }
    ResourceRef<NifResInterpreter> hold(res);

    res->val = new tflite::Interpreter();
    ret = enif_make_resource(env, res);
    return erlang::nif::ok(env, ret);
}

// Asking who controls an interpreter touches none of its state, and a process
// that has just handed one away still has a reason to ask. So this one does not
// go through get_resource, which would refuse everybody but the controller --
// ets:info(Table, owner) answers anyone too.
ERL_NIF_TERM interpreter_controlling_process(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResInterpreter * self_res;

    if (!enif_get_resource(env, argv[0], NifResInterpreter::type, (void **)&self_res) || self_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResInterpreter resource");
    }

    if (!self_res->is_controlled) {
        return erlang::nif::atom(env, "undefined");
    }
    return erlang::nif::ok(env, enif_make_pid(env, &self_res->controlling_process));
}

// Not routed through get_resource: that refuses anyone but the controlling
// process, and handing control on is precisely a call the controlling process
// makes about somebody else.
ERL_NIF_TERM interpreter_set_controlling_process(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    if (!enif_get_resource(env, argv[0], NifResInterpreter::type, (void **)&self_res) || self_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResInterpreter resource");
    }

    ErlNifPid caller;
    if (enif_self(env, &caller) == nullptr) {
        return erlang::nif::error(env, "cannot identify the calling process");
    }

    // gen_tcp's rule: with no controlling process anyone may take it, and with
    // one only that process may hand it on
    if (self_res->is_controlled &&
        enif_compare_pids(&caller, &self_res->controlling_process) != 0 &&
        enif_is_process_alive(env, &self_res->controlling_process)) {
        return erlang::nif::error(env, "interpreter belongs to another process");
    }

    ErlNifPid target;
    if (enif_get_local_pid(env, argv[1], &target)) {
        if (!enif_is_process_alive(env, &target)) {
            return erlang::nif::error(env, "the given process is not alive");
        }
        self_res->controlling_process = target;
        self_res->is_controlled = true;
        return erlang::nif::ok(env);
    }

    std::string undefined;
    if (erlang::nif::get_atom(env, argv[1], undefined) && undefined == "undefined") {
        self_res->is_controlled = false;
        return erlang::nif::ok(env);
    }

    return erlang::nif::error(env, "expecting a local pid or undefined");
}

ERL_NIF_TERM interpreter_set_inputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM inputs_nif = argv[1];
    NifResInterpreter * self_res;
    std::vector<int> inputs;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!erlang::nif::get_list(env, inputs_nif, inputs)) {
        return erlang::nif::error(env, "expecting `inputs` to be a list of non-negative integers");
    }

    TfLiteStatus status = self_res->val->SetInputs(inputs);
    return tflite_status_to_erl_term(env, status);
}

// The strict and non-strict resizes differ by one method name, so they share a
// body the way the signature runner's pair already does.
static ERL_NIF_TERM _resize(ErlNifEnv *env, const ERL_NIF_TERM argv[], bool strict) {
    NifResInterpreter * self_res;
    int tensor_index;
    std::vector<int> dims;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!erlang::nif::get(env, argv[1], &tensor_index)) {
        return erlang::nif::error(env, "expecting `tensor_index` to be an integer");
    }

    if (!erlang::nif::get_list(env, argv[2], dims)) {
        return erlang::nif::error(env, "expecting `dims` to be a list of non-negative integers");
    }

    TfLiteStatus status = strict
        ? self_res->val->ResizeInputTensorStrict(tensor_index, dims)
        : self_res->val->ResizeInputTensor(tensor_index, dims);
    // Retires outstanding tensor handles; see interpreter_allocate_tensors.
    NifResInterpreter::release_tensors(self_res);
    return tflite_status_to_erl_term(env, status);
}

ERL_NIF_TERM interpreter_resize_input_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);
    return _resize(env, argv, false);
}

ERL_NIF_TERM interpreter_resize_input_tensor_strict(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);
    return _resize(env, argv, true);
}


ERL_NIF_TERM interpreter_enable_cancellation(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    return tflite_status_to_erl_term(env, self_res->val->EnableCancellation());
}

ERL_NIF_TERM interpreter_cancel(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    // No in_use here, deliberately: cancel exists to be called from another
    // process while invoke holds that guard, so taking it would mean cancel only
    // worked when there was nothing to cancel. It still has to be kept away from
    // the one call that deletes what it is about to dereference.
    TFLITE_BEAM_INTERPRETER_NOT_BEING_REPLACED(self_res);

    // and re-read val after it, for the same reason: get_resource saw it before
    // the lock, and a rebuild that failed leaves it null
    if (self_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResInterpreter resource");
    }

    return tflite_status_to_erl_term(env, self_res->val->Cancel());
}

ERL_NIF_TERM interpreter_release_non_persistent_memory(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    return tflite_status_to_erl_term(env, self_res->val->ReleaseNonPersistentMemory());
}

ERL_NIF_TERM interpreter_reset_variable_tensors(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    return tflite_status_to_erl_term(env, self_res->val->ResetVariableTensors());
}

ERL_NIF_TERM interpreter_subgraphs_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    return erlang::nif::ok(env, enif_make_uint64(env, self_res->val->subgraphs_size()));
}

ERL_NIF_TERM interpreter_get_allow_fp16_precision_for_fp32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    return erlang::nif::ok(env, self_res->val->GetAllowFp16PrecisionForFp32() ? erlang::nif::atom(env, "true") : erlang::nif::atom(env, "false"));
}

ERL_NIF_TERM interpreter_set_allow_fp16_precision_for_fp32(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    ERL_NIF_TERM ret;
    std::string allow;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!erlang::nif::get_atom(env, argv[1], allow) || (allow != "true" && allow != "false")) {
        return erlang::nif::error(env, "expecting `allow` to be a boolean");
    }

    self_res->val->SetAllowFp16PrecisionForFp32(allow == "true");
    return erlang::nif::ok(env);
}

static ERL_NIF_TERM _signature_tensor_map(ErlNifEnv *env, const std::map<std::string, uint32_t>& m) {
    ERL_NIF_TERM map = enif_make_new_map(env);
    for (const auto& kv : m) {
        enif_make_map_put(env, map,
            erlang::nif::make_binary(env, kv.first.c_str()),
            enif_make_uint(env, kv.second), &map);
    }
    return map;
}

ERL_NIF_TERM interpreter_signature_inputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    std::string signature_key;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!erlang::nif::get(env, argv[1], signature_key)) {
        return erlang::nif::error(env, "expecting `signature_key` to be a string");
    }

    return erlang::nif::ok(env, _signature_tensor_map(env, self_res->val->signature_inputs(signature_key.c_str())));
}

ERL_NIF_TERM interpreter_signature_outputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    std::string signature_key;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!erlang::nif::get(env, argv[1], signature_key)) {
        return erlang::nif::error(env, "expecting `signature_key` to be a string");
    }

    return erlang::nif::ok(env, _signature_tensor_map(env, self_res->val->signature_outputs(signature_key.c_str())));
}

ERL_NIF_TERM interpreter_get_subgraph_index_from_signature(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    NifResInterpreter * self_res;
    std::string signature_key;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, argv[0], ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!erlang::nif::get(env, argv[1], signature_key)) {
        return erlang::nif::error(env, "expecting `signature_key` to be a string");
    }

    return erlang::nif::ok(env, enif_make_int(env, self_res->val->GetSubgraphIndexFromSignature(signature_key.c_str())));
}

ERL_NIF_TERM interpreter_set_outputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM outputs_nif = argv[1];
    NifResInterpreter * self_res;
    std::vector<int> outputs;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!erlang::nif::get_list(env, outputs_nif, outputs)) {
        return erlang::nif::error(env, "expecting `outputs` to be a list of non-negative integers");
    }

    TfLiteStatus status = self_res->val->SetOutputs(outputs);
    return tflite_status_to_erl_term(env, status);
}

ERL_NIF_TERM interpreter_set_variables(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM variables_nif = argv[1];
    NifResInterpreter * self_res;
    std::vector<int> variables;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!erlang::nif::get_list(env, variables_nif, variables)) {
        return erlang::nif::error(env, "expecting `variables` to be a list of non-negative integers");
    }

    TfLiteStatus status = self_res->val->SetVariables(variables);
    return tflite_status_to_erl_term(env, status);
}

ERL_NIF_TERM interpreter_inputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    const std::vector<int>& inputs = self_res->val->inputs();
    if (erlang::nif::make(env, inputs, ret)) {
        return erlang::nif::error(env, "enif_alloc failed");
    }

    return erlang::nif::ok(env, ret);
}

ERL_NIF_TERM interpreter_get_input_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM index_nif = argv[1];
    int index;
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!enif_get_int(env, index_nif, &index)) {
        return erlang::nif::error(env, "expecting index to be an integer");
    }

    const auto& inputs = self_res->val->inputs();
    if (inputs.size() <= index || index < 0) {
        return erlang::nif::error(env, "index out of bound");
    }

    const char * name = self_res->val->GetInputName(index);
    if (name == nullptr) {
        return erlang::nif::error(env, "cannot get tensor's name");
    }

    return erlang::nif::ok(env, erlang::nif::make_binary(env, name));
}

ERL_NIF_TERM interpreter_outputs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    const std::vector<int>& outputs = self_res->val->outputs();
    if (erlang::nif::make(env, outputs, ret)) {
        return erlang::nif::error(env, "enif_alloc failed");
    }

    return erlang::nif::ok(env, ret);
}

ERL_NIF_TERM interpreter_variables(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    const std::vector<int>& variables = self_res->val->variables();
    if (erlang::nif::make(env, variables, ret)) {
        return erlang::nif::error(env, "enif_alloc failed");
    }

    return erlang::nif::ok(env, ret);
}

ERL_NIF_TERM interpreter_get_output_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM index_nif = argv[1];
    int index;
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!enif_get_int(env, index_nif, &index)) {
        return erlang::nif::error(env, "expecting index to be an integer");
    }

    const auto& outputs = self_res->val->outputs();
    if (outputs.size() <= index || index < 0) {
        return erlang::nif::error(env, "index out of bound");
    }

    const char * name = self_res->val->GetOutputName(index);
    if (name == nullptr) {
        return erlang::nif::error(env, "cannot get tensor's name");
    }

    return erlang::nif::ok(env, erlang::nif::make_binary(env, name));
}

ERL_NIF_TERM interpreter_tensors_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    return erlang::nif::ok(env, enif_make_uint64(env, self_res->val->tensors_size()));
}

ERL_NIF_TERM interpreter_nodes_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    return erlang::nif::ok(env, enif_make_uint64(env, self_res->val->nodes_size()));
}

ERL_NIF_TERM interpreter_execution_plan(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    const std::vector<int>& execution_plan = self_res->val->execution_plan();
    if (erlang::nif::make(env, execution_plan, ret)) {
        return erlang::nif::error(env, "enif_alloc failed");
    }

    return erlang::nif::ok(env, ret);
}

ERL_NIF_TERM interpreter_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM index_nif = argv[1];
    int index;
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!enif_get_int(env, index_nif, &index)) {
        return erlang::nif::error(env, "expecting index to be an integer");
    }

    const size_t num_tensors = self_res->val->tensors_size();
    if (num_tensors <= index || index < 0) {
        return erlang::nif::error(env, "index out of bound");
    }

    NifResTfLiteTensor * tensor_res = nullptr;
    if (!(tensor_res = NifResTfLiteTensor::allocate_resource(env, ret))) {
        return ret;
    }
    ResourceRef<NifResTfLiteTensor> hold(tensor_res);

    tensor_res->val = self_res->val->tensor(index);
    tensor_res->index = index;

    ERL_NIF_TERM tensor_type;
    if (!_tflitetensor_type(env, tensor_res->val, tensor_type)) {
        tensor_type = erlang::nif::atom(env, "unknown");
    }

    ERL_NIF_TERM tensor_shape;
    if (!_tflitetensor_shape(env, tensor_res->val, tensor_shape)) {
        return erlang::nif::error(env, "cannot allocate memory for tensor shape");
    }

    ERL_NIF_TERM tensor_shape_signature;
    if (!_tflitetensor_shape_signature(env, tensor_res->val, tensor_shape_signature)) {
        return erlang::nif::error(env, "cannot allocate memory for tensor shape signature");
    }

    ERL_NIF_TERM tensor_name;
    if (!_tflitetensor_name(env, tensor_res->val, tensor_name)) {
        return erlang::nif::error(env, "cannot allocate memory for tensor name");
    }

    ERL_NIF_TERM tensor_quantization_params;
    if (!_tflitetensor_quantization_params(env, tensor_res->val, tensor_quantization_params)) {
        return erlang::nif::error(env, "cannot allocate memory for tensor quantization params");
    }

    ERL_NIF_TERM tensor_sparsity_params;
    if (!_tflitetensor_sparsity_params(env, tensor_res->val, tensor_sparsity_params)) {
        return erlang::nif::error(env, "cannot allocate memory for tensor sparsity params");
    }

    // The handle is what keeps the interpreter alive. Erlang stops counting a
    // variable as live at its last mention, so an interpreter that a caller
    // fetched a tensor from and then never named again is collectable, and
    // without this the tensor would be left pointing into a freed arena.
    tensor_res->interpreter = self_res;
    enif_keep_resource(self_res);

    // Registered so that allocate_tensors, a reshape, or a rebuild can retire
    // this handle. The registry holds a bare pointer and takes no reference; the
    // handle removes itself in its destructor.
    if (self_res->tensors) {
        MutexLock registry(self_res->tensors_lock);
        self_res->tensors->push_back(tensor_res);
    }

    ERL_NIF_TERM tensor_reference = enif_make_resource(env, tensor_res);

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
}

ERL_NIF_TERM interpreter_signature_keys(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    const std::vector<const std::string*> signature_keys = self_res->val->signature_keys();
    if (erlang::nif::make(env, signature_keys, ret)) {
        return erlang::nif::error(env, "enif_alloc failed");
    }

    return erlang::nif::ok(env, ret);
}

ERL_NIF_TERM interpreter_input_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM index_nif = argv[1];
    ERL_NIF_TERM data_nif = argv[2];
    int index;
    ErlNifBinary data;
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!enif_get_int(env, index_nif, &index)) {
        return erlang::nif::error(env, "expecting index to be an integer");
    }

    if (!enif_inspect_binary(env, data_nif, &data)) {
        return erlang::nif::error(env, "cannot get input data");
    }

    const auto& inputs = self_res->val->inputs();
    if (inputs.size() <= index || index < 0) {
        return erlang::nif::error(env, "index out of bound");
    }

    auto input_tensor = self_res->val->input_tensor(index);
    if (input_tensor->data.data == nullptr) {
        return erlang::nif::error(env, "tensor is not allocated yet? Please call TFLiteBEAM.Interpreter.allocate_tensors first");
    }

    size_t maximum_bytes = input_tensor->bytes;
    if (data.size < maximum_bytes) {
        maximum_bytes = data.size;
    }
    memcpy(input_tensor->data.data, data.data, maximum_bytes);
    return erlang::nif::ok(env);
}

ERL_NIF_TERM interpreter_output_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM index_nif = argv[1];
    int index;
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!enif_get_int(env, index_nif, &index)) {
        return erlang::nif::error(env, "expecting index to be an integer");
    }

    const auto& outputs = self_res->val->outputs();
    if (outputs.size() <= index || index < 0) {
        return erlang::nif::error(env, "index out of bound");
    }

    auto t = self_res->val->output_tensor(index);
    if (t->data.data == nullptr) {
        return erlang::nif::error(env, "tensor is not allocated yet? Please call TFLiteBEAM.Interpreter.allocate_tensors first");
    }

    ErlNifBinary tensor_data;
    size_t tensor_size = t->bytes;
    if (!enif_alloc_binary(tensor_size, &tensor_data)) {
        return erlang::nif::error(env, "cannot allocate enough memory for the tensor");
    }

    memcpy(tensor_data.data, t->data.data, tensor_size);
    return erlang::nif::ok(env, enif_make_binary(env, &tensor_data));
}

ERL_NIF_TERM interpreter_allocate_tensors(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResInterpreter * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    TfLiteStatus status = self_res->val->AllocateTensors();
    // Any TfLiteTensor a caller is already holding points into storage this
    // call can move, so retire those handles here. release_tensors flags them
    // so a later use reports the interpreter changed underneath rather than
    // reading or writing the old address, which is what get_resource checks.
    NifResInterpreter::release_tensors(self_res);
    return tflite_status_to_erl_term(env, status);
}

ERL_NIF_TERM interpreter_get_signature_defs(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResInterpreter * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    auto interpreter_ = self_res->val;
    ERL_NIF_TERM result;

    size_t num_items = interpreter_->signature_keys().size();
    if (num_items == 0) {
        return erlang::nif::ok(env, erlang::nif::atom(env, "nil"));
    }

    ERL_NIF_TERM * keys = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * num_items);
    if (keys == nullptr) {
        return erlang::nif::error(env, "out of memory");
    }
    ERL_NIF_TERM * vals = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * num_items);
    if (vals == nullptr) {
        enif_free(keys);
        return erlang::nif::error(env, "out of memory");
    }

    size_t sig_key_index = 0;
    ERL_NIF_TERM signature_def_keys[2];
    signature_def_keys[0] = erlang::nif::atom(env, "inputs");
    signature_def_keys[1] = erlang::nif::atom(env, "outputs");
    for (const auto& sig_key : interpreter_->signature_keys()) {
        ERL_NIF_TERM signature_def_vals[2];
        const auto& signature_def_inputs = interpreter_->signature_inputs(sig_key->c_str());
        const auto& signature_def_outputs = interpreter_->signature_outputs(sig_key->c_str());

        size_t inputs_items = signature_def_inputs.size();
        ERL_NIF_TERM * inputs_keys = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * inputs_items);
        if (inputs_keys == nullptr) {
            enif_free(keys);
            enif_free(vals);
            return erlang::nif::error(env, "out of memory");
        }
        ERL_NIF_TERM * inputs_vals = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * inputs_items);
        if (inputs_vals == nullptr) {
            enif_free(keys);
            enif_free(vals);
            enif_free(inputs_keys);
            return erlang::nif::error(env, "out of memory");
        }

        size_t input_item_index = 0;
        for (const auto& input : signature_def_inputs) {
            if (input.first.length() > 0) {
                inputs_keys[input_item_index] = erlang::nif::atom(env, input.first.c_str());
                inputs_vals[input_item_index] = erlang::nif::make(env, (long)input.second);
                input_item_index++;
            }
        }
        if (!enif_make_map_from_arrays(env, inputs_keys, inputs_vals, input_item_index, &signature_def_vals[0])) {
            enif_free(keys);
            enif_free(vals);
            enif_free(inputs_keys);
            enif_free(inputs_vals);
            return erlang::nif::error(env, "duplicate keys found in signature_def_inputs");
        }
        enif_free(inputs_keys);
        enif_free(inputs_vals);

        size_t outputs_items = signature_def_outputs.size();
        ERL_NIF_TERM * outputs_keys = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * outputs_items);
        if (outputs_keys == nullptr) {
            enif_free(keys);
            enif_free(vals);
            return erlang::nif::error(env, "out of memory");
        }
        ERL_NIF_TERM * outputs_vals = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * outputs_items);
        if (outputs_vals == nullptr) {
            enif_free(keys);
            enif_free(vals);
            enif_free(outputs_keys);
            return erlang::nif::error(env, "out of memory");
        }
        size_t output_item_index = 0;
        for (const auto& output : signature_def_outputs) {
            if (output.first.length()) {
                outputs_keys[output_item_index] = erlang::nif::atom(env, output.first.c_str());
                outputs_vals[output_item_index] = erlang::nif::make(env, (long)output.second);
                output_item_index++;
            }
        }
        if (!enif_make_map_from_arrays(env, outputs_keys, outputs_vals, output_item_index, &signature_def_vals[1])) {
            enif_free(keys);
            enif_free(vals);
            enif_free(outputs_keys);
            enif_free(outputs_vals);
            return erlang::nif::error(env, "duplicate keys found in signature_def_outputs");
        }
        enif_free(outputs_keys);
        enif_free(outputs_vals);

        keys[sig_key_index] = erlang::nif::atom(env, sig_key->c_str());
        enif_make_map_from_arrays(env, signature_def_keys, signature_def_vals, 2, &vals[sig_key_index]);
        sig_key_index++;
    }

    enif_make_map_from_arrays(env, keys, vals, num_items, &result);
    enif_free(keys);
    enif_free(vals);
    return erlang::nif::ok(env, result);
}

ERL_NIF_TERM interpreter_invoke(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResInterpreter *self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    auto status = self_res->val->Invoke();
    // Invoke is named in TfLite's own warning about the pointers tensor()
    // returns, so any handle whose tensor moved during inference is retired here.
    NifResInterpreter::revalidate_tensors(self_res);
    return tflite_status_to_erl_term(env, status);
}

ERL_NIF_TERM interpreter_set_num_threads(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    ERL_NIF_TERM num_threads_nif = argv[1];
    int num_threads = 1;
    NifResInterpreter * self_res;
    ERL_NIF_TERM ret;

    if (!(self_res = NifResInterpreter::get_resource(env, self_nif, ret))) {
        return ret;
    }

    TFLITE_BEAM_INTERPRETER_IN_USE(self_res);

    if (!enif_get_int(env, num_threads_nif, &num_threads) || num_threads < 1) {
        return erlang::nif::error(env, "expecting num_threads to be an positive integer");
    }

    auto status = self_res->val->SetNumThreads(num_threads);
    return tflite_status_to_erl_term(env, status);
}
