#include <erl_nif.h>
#include "nif_utils.hpp"

#include "tensorflow/lite/c/c_api.h"
#include "tensorflow/lite/c/common.h"

#include "erlang_nif_resource.h"

NifResBuiltinOpResolver * NifResBuiltinOpResolver::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResBuiltinOpResolver * res = (NifResBuiltinOpResolver *)enif_alloc_resource(NifResBuiltinOpResolver::type, sizeof(NifResBuiltinOpResolver));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResBuiltinOpResolver resource");
        return res;
    }

    res->val = nullptr;
    res->apply_default_delegates = true;

    return res;
}

NifResBuiltinOpResolver * NifResBuiltinOpResolver::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResBuiltinOpResolver * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResBuiltinOpResolver::type, (void **)&self_res) || self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResBuiltinOpResolver resource");
        return nullptr;
    }
    return self_res;
}

void NifResBuiltinOpResolver::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResBuiltinOpResolver *)args;
    if (res && res->val) {
        delete res->val;
        res->val = nullptr;
    }
}

NifResErrorReporter * NifResErrorReporter::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResErrorReporter * res = (NifResErrorReporter *)enif_alloc_resource(NifResErrorReporter::type, sizeof(NifResErrorReporter));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResErrorReporter resource");
        return res;
    }

    res->val = nullptr;
    res->is_default = false;

    return res;
}

NifResErrorReporter * NifResErrorReporter::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResErrorReporter * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResErrorReporter::type, (void **)&self_res) || self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResErrorReporter resource");
        return nullptr;
    }
    return self_res;
}

void NifResErrorReporter::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResErrorReporter *)args;
    if (res) {
        if (res->val) {
            if (!res->is_default || res->val != tflite::DefaultErrorReporter()) {
                delete res->val;
            }
            res->val = nullptr;
        }
    }
}

NifResFlatBufferModel * NifResFlatBufferModel::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResFlatBufferModel * res = (NifResFlatBufferModel *)enif_alloc_resource(NifResFlatBufferModel::type, sizeof(NifResFlatBufferModel));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResFlatBufferModel resource");
        return res;
    }
    
    res->val = nullptr;
    res->copied_buffer = nullptr;

    return res;
}

NifResFlatBufferModel * NifResFlatBufferModel::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResFlatBufferModel * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResFlatBufferModel::type, (void **)&self_res) || self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResFlatBufferModel resource");
        return nullptr;
    }
    return self_res;
}

void NifResFlatBufferModel::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResFlatBufferModel *)args;
    if (res) {
        if (res->val) {
            delete res->val;
            res->val = nullptr;
        }
        if (res->copied_buffer) {
            enif_free((void *)res->copied_buffer);
            res->copied_buffer = nullptr;
        }
    }
}

NifResDelegate * NifResDelegate::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResDelegate * res = (NifResDelegate *)enif_alloc_resource(NifResDelegate::type, sizeof(NifResDelegate));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResDelegate resource");
        return res;
    }

    res->val = nullptr;
    res->deleter = nullptr;
    res->owned_path = nullptr;

    return res;
}

NifResDelegate * NifResDelegate::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResDelegate * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResDelegate::type, (void **)&self_res) || self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResDelegate resource");
        return nullptr;
    }
    return self_res;
}

void NifResDelegate::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResDelegate *)args;
    if (res) {
        // a delegate comes from a C factory and has to go back through the
        // matching C destructor. Several of those dereference their argument,
        // so a delegate that was never constructed is simply dropped.
        if (res->val && res->deleter) {
            res->deleter(res->val);
        }
        res->val = nullptr;
        res->deleter = nullptr;

        if (res->owned_path) {
            enif_free(res->owned_path);
            res->owned_path = nullptr;
        }
    }
}

NifResInterpreterBuilder * NifResInterpreterBuilder::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResInterpreterBuilder * res = (NifResInterpreterBuilder *)enif_alloc_resource(NifResInterpreterBuilder::type, sizeof(NifResInterpreterBuilder));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResInterpreterBuilder resource");
        return res;
    }

    res->val = nullptr;
    res->op_resolver = nullptr;
    res->flatbuffer_model = nullptr;
    res->delegates = new std::vector<NifResDelegateEntry>;
    res->num_threads = -1;

    return res;
}

NifResInterpreterBuilder * NifResInterpreterBuilder::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResInterpreterBuilder * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResInterpreterBuilder::type, (void **)&self_res) || self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResInterpreterBuilder resource");
        return nullptr;
    }
    return self_res;
}

void NifResInterpreterBuilder::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResInterpreterBuilder *)args;
    if (res) {
        if (res->val) {
            delete res->val;
            res->val = nullptr;
        }

        if (res->op_resolver) {
            enif_release_resource(res->op_resolver);
            res->op_resolver = nullptr;
        }

        if (res->flatbuffer_model) {
            enif_release_resource(res->flatbuffer_model);
            res->flatbuffer_model = nullptr;
        }

        if (res->delegates) {
            for (auto & entry : *res->delegates) {
                if (entry.delegate) enif_release_resource(entry.delegate);
            }
            delete res->delegates;
            res->delegates = nullptr;
        }
    }
}

NifResInterpreter * NifResInterpreter::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResInterpreter * res = (NifResInterpreter *)enif_alloc_resource(NifResInterpreter::type, sizeof(NifResInterpreter));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResInterpreter resource");
        return res;
    }

    res->val = nullptr;
    res->flatbuffer_model = nullptr;
    res->edgetpu_context = nullptr;
    res->tensors = new std::map<int, NifResTfLiteTensor *>;
    res->signature_runners = new std::vector<NifResSignatureRunner *>;
    res->delegates = new std::vector<NifResDelegate *>;
    res->in_use = enif_mutex_create((char *)"tflite_beam_interpreter");
    res->is_controlled = false;

    return res;
}

// Whether the calling process is allowed near this interpreter at all. An
// interpreter nobody has claimed is open to everyone, which is how they have
// always behaved and what keeps this opt-in.
bool caller_may_use(ErlNifEnv * env, NifResInterpreter * res) {
    if (res == nullptr || !res->is_controlled) return true;

    ErlNifPid caller;
    if (enif_self(env, &caller) == nullptr) return false;
    if (enif_compare_pids(&caller, &res->controlling_process) == 0) return true;

    // a controlling process that has died leaves the interpreter to whoever
    // wants it: there is no equivalent here of closing a socket
    if (enif_is_process_alive(env, &res->controlling_process)) return false;

    res->is_controlled = false;
    return true;
}

NifResInterpreter * NifResInterpreter::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResInterpreter * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResInterpreter::type, (void **)&self_res) || self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResInterpreter resource");
        return nullptr;
    }

    // one check for all thirty-three call sites, rather than thirty-three
    // chances to forget one
    if (!caller_may_use(env, self_res)) {
        error = erlang::nif::error(env, "interpreter belongs to another process");
        return nullptr;
    }

    return self_res;
}

void NifResInterpreter::release_tensors(NifResInterpreter * res) {
    if (res == nullptr || res->tensors == nullptr) return;

    for (auto tensor_res_pair : *res->tensors) {
        auto tensor_res = tensor_res_pair.second;
        if (tensor_res) {
            tensor_res->interpreter_has_gone = true;
            enif_release_resource(tensor_res);
        }
    }
    res->tensors->clear();
}

void NifResInterpreter::release_signature_runners(NifResInterpreter * res) {
    if (res == nullptr || res->signature_runners == nullptr) return;

    // Flag only: the registry never took a reference, so there is none to give
    // back. Whoever holds the runner in Erlang still holds it, and now finds out
    // that what it borrows from is gone.
    for (auto runner_res : *res->signature_runners) {
        if (runner_res) {
            runner_res->interpreter_has_gone = true;
        }
    }
    res->signature_runners->clear();
}

void NifResInterpreter::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResInterpreter *)args;
    if (res) {
        NifResInterpreter::release_tensors(res);
        if (res->tensors) {
            delete res->tensors;
            res->tensors = nullptr;
        }

        NifResInterpreter::release_signature_runners(res);
        if (res->signature_runners) {
            delete res->signature_runners;
            res->signature_runners = nullptr;
        }

        if (res->val) {
            delete res->val;
            res->val = nullptr;
        }

        if (res->flatbuffer_model) {
            enif_release_resource(res->flatbuffer_model);
            res->flatbuffer_model = nullptr;
        }

        if (res->edgetpu_context) {
            enif_release_resource(res->edgetpu_context);
            res->edgetpu_context = nullptr;
        }

        if (res->in_use) {
            enif_mutex_destroy(res->in_use);
            res->in_use = nullptr;
        }

        // after the interpreter itself: a delegate has to outlive the graph it
        // was applied to
        if (res->delegates) {
            for (auto delegate_res : *res->delegates) {
                if (delegate_res) enif_release_resource(delegate_res);
            }
            delete res->delegates;
            res->delegates = nullptr;
        }
    }
}

NifResSignatureRunner * NifResSignatureRunner::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResSignatureRunner * res = (NifResSignatureRunner *)enif_alloc_resource(NifResSignatureRunner::type, sizeof(NifResSignatureRunner));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResSignatureRunner resource");
        return res;
    }

    res->val = nullptr;
    res->interpreter = nullptr;
    res->interpreter_has_gone = false;

    return res;
}

NifResSignatureRunner * NifResSignatureRunner::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResSignatureRunner * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResSignatureRunner::type, (void **)&self_res) || self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResSignatureRunner resource");
        return nullptr;
    }

    if (self_res->interpreter_has_gone) {
        error = erlang::nif::error(env, "cannot access NifResSignatureRunner resource: the interpreter it came from has been rebuilt");
        return nullptr;
    }

    // a runner is a view onto its interpreter and runs on its subgraph, so it
    // answers to whichever process controls that interpreter
    if (!caller_may_use(env, self_res->interpreter)) {
        error = erlang::nif::error(env, "interpreter belongs to another process");
        return nullptr;
    }

    return self_res;
}

void NifResSignatureRunner::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResSignatureRunner *)args;
    if (res) {
        // the interpreter owns the runner, so only the pointer is dropped here
        res->val = nullptr;

        // Out of the registry before the interpreter reference goes, or the
        // registry would be left holding an address that has just been freed.
        if (res->interpreter && res->interpreter->signature_runners) {
            auto & list = *res->interpreter->signature_runners;
            for (auto it = list.begin(); it != list.end(); ++it) {
                if (*it == res) {
                    list.erase(it);
                    break;
                }
            }
        }

        if (res->interpreter) {
            enif_release_resource(res->interpreter);
            res->interpreter = nullptr;
        }
    }
}

NifResTfLiteTensor * NifResTfLiteTensor::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResTfLiteTensor * res = (NifResTfLiteTensor *)enif_alloc_resource(NifResTfLiteTensor::type, sizeof(NifResTfLiteTensor));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResTfLiteTensor resource");
        return res;
    }

    res->val = nullptr;
    res->borrowed = false;
    res->interpreter_has_gone = false;

    return res;
}

NifResTfLiteTensor * NifResTfLiteTensor::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResTfLiteTensor * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResTfLiteTensor::type, (void **)&self_res) || self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResTfLiteTensor resource");
        return nullptr;
    }

    if (self_res->interpreter_has_gone) {
        error = erlang::nif::error(env, "cannot access NifResTfLiteTensor resource: associcated interpreter has been dropped");
        return nullptr;
    }

    return self_res;
}

void NifResTfLiteTensor::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResTfLiteTensor *)args;
    if (res) {
        if (res->val) {
            if (!res->borrowed) {
                delete res->val;
                res->val = nullptr;
            }
        }
    }
}

#ifdef CORAL_SUPPORT_ENABLED

NifResEdgeTpuContext * NifResEdgeTpuContext::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResEdgeTpuContext * res = (NifResEdgeTpuContext *)enif_alloc_resource(NifResEdgeTpuContext::type, sizeof(NifResEdgeTpuContext));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResEdgeTpuContext resource");
        return res;
    }

    res->val = nullptr;
    res->context = nullptr;

    return res;
}

NifResEdgeTpuContext * NifResEdgeTpuContext::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResEdgeTpuContext * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResEdgeTpuContext::type, (void **)&self_res) || self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResEdgeTpuContext resource");
        return nullptr;
    }
    return self_res;
}

void NifResEdgeTpuContext::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResEdgeTpuContext *)args;
    if (res) {
        // dropping the last share of the context is what releases the device
        delete res->context;
        res->context = nullptr;
        res->val = nullptr;
    }
}

#endif
