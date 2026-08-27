#include <erl_nif.h>
#include "nif_utils.hpp"

#include "tflite/c/c_api.h"
#include "tflite/c/common.h"

#include "erlang_nif_resource.h"
#include "fault_inject.hpp"

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
        // DefaultErrorReporter is a function-local static, so the pointer is
        // the whole question: anything else was made here and is ours to delete.
        if (res->val) {
            if (res->val != tflite::DefaultErrorReporter()) {
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
    res->error_reporter = nullptr;

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
        // after the model, since destroying it can still report through this
        if (res->error_reporter) {
            enif_release_resource(res->error_reporter);
            res->error_reporter = nullptr;
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
    res->delegates = nullptr;
    res->num_threads = -1;

    // The reference enif_alloc_resource just took is given away by the caller,
    // when it turns this into a term. Until then a failure is a resource nothing
    // will ever collect, so anything that can fail hands the reference back
    // before returning, and a builder that exists at all has its containers.
    try {
        erlang::nif::fault_point(erlang::nif::kFaultBuilderContainers);
        res->delegates = new std::vector<NifResDelegateEntry>;
    } catch (const std::bad_alloc &) {
        enif_release_resource(res);
        error = erlang::nif::error(env, "cannot allocate NifResInterpreterBuilder resource");
        return nullptr;
    }

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
    res->tensors = nullptr;
    res->signature_runners = nullptr;
    res->tensors_lock = nullptr;
    res->signature_runners_lock = nullptr;
    res->delegates = nullptr;
    res->in_use = nullptr;
    res->being_replaced = nullptr;
    res->is_controlled = false;

    // All or nothing, for the same reason as the builder above and one more: an
    // interpreter missing its in_use mutex would answer every guarded call with
    // "already in use by another process", which is a lockout dressed up as a
    // collision. Better to fail the allocation and say so.
    try {
        erlang::nif::fault_point(erlang::nif::kFaultInterpreterContainers);
        res->tensors = new std::vector<NifResTfLiteTensor *>;
        res->signature_runners = new std::vector<NifResSignatureRunner *>;
        res->delegates = new std::vector<NifResDelegate *>;
    } catch (const std::bad_alloc &) {
        enif_release_resource(res);
        error = erlang::nif::error(env, "cannot allocate NifResInterpreter resource");
        return nullptr;
    }

    res->tensors_lock = enif_mutex_create((char *)"tflite_beam_tensors");
    res->signature_runners_lock = enif_mutex_create((char *)"tflite_beam_signature_runners");
    res->in_use = enif_mutex_create((char *)"tflite_beam_interpreter");
    res->being_replaced = enif_mutex_create((char *)"tflite_beam_interpreter_replace");
    if (res->tensors_lock == nullptr || res->signature_runners_lock == nullptr ||
        res->in_use == nullptr || res->being_replaced == nullptr) {
        enif_release_resource(res);
        error = erlang::nif::error(env, "cannot allocate NifResInterpreter resource");
        return nullptr;
    }

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

    // Flag only: the registry never took a reference, so there is none to give
    // back. Whoever holds the handle in Erlang still holds it, and now finds out
    // that what it borrows from has moved.
    MutexLock registry(res->tensors_lock);
    for (auto tensor_res : *res->tensors) {
        if (tensor_res) {
            tensor_res->interpreter_has_gone = true;
        }
    }
    res->tensors->clear();
}

// For the calls that may move tensors rather than existing to move them.
// Upstream is explicit that Invoke and AddTensors "may invalidate the pointer
// returned by this function", and that the list is not exhaustive, so the choice
// is between retiring every handle after every invoke and asking whether
// anything actually moved. Retiring them all would make a handle useless: fetch,
// set, invoke, read is the ordinary sequence and the read would always fail.
// Asking costs one pointer compare per live handle.
void NifResInterpreter::revalidate_tensors(NifResInterpreter * res) {
    if (res == nullptr || res->tensors == nullptr || res->val == nullptr) return;

    MutexLock registry(res->tensors_lock);
    const size_t count = res->val->tensors_size();
    for (auto tensor_res : *res->tensors) {
        if (tensor_res == nullptr || tensor_res->interpreter_has_gone) continue;
        const int index = tensor_res->index;
        if (index < 0 || (size_t)index >= count ||
            res->val->tensor(index) != tensor_res->val) {
            tensor_res->interpreter_has_gone = true;
        }
    }
}

void NifResInterpreter::release_signature_runners(NifResInterpreter * res) {
    if (res == nullptr || res->signature_runners == nullptr) return;

    // Flag only: the registry never took a reference, so there is none to give
    // back. Whoever holds the runner in Erlang still holds it, and now finds out
    // that what it borrows from is gone.
    MutexLock registry(res->signature_runners_lock);
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
        if (res->tensors_lock) {
            enif_mutex_destroy(res->tensors_lock);
            res->tensors_lock = nullptr;
        }

        if (res->signature_runners_lock) {
            enif_mutex_destroy(res->signature_runners_lock);
            res->signature_runners_lock = nullptr;
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

        if (res->being_replaced) {
            enif_mutex_destroy(res->being_replaced);
            res->being_replaced = nullptr;
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
        error = erlang::nif::error(env, "cannot access NifResSignatureRunner resource: the handle has been retired, because the interpreter it came from was rebuilt");
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
        // This runs on whichever thread dropped the last reference, so it takes
        // the registry lock like every other writer.
        if (res->interpreter && res->interpreter->signature_runners) {
            MutexLock registry(res->interpreter->signature_runners_lock);
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
    res->interpreter = nullptr;
    res->index = -1;
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
        error = erlang::nif::error(env, "cannot access NifResTfLiteTensor resource: the handle has been retired, because the interpreter moved its tensors");
        return nullptr;
    }

    // A handle is a way into its interpreter's arena, so it answers to whoever
    // controls that interpreter, exactly as the interpreter and its signature
    // runners do. Without this the guard was on the door and not on the window:
    // interpreter:tensor/2 refused a process that did not own the interpreter,
    // and a handle that process already held read and wrote through it anyway,
    // with the owner seeing the foreign write. tflite_beam_interpreter_server
    // hands out such a handle from with/2, so its isolation was undone by its
    // own escape hatch.
    if (!caller_may_use(env, self_res->interpreter)) {
        error = erlang::nif::error(env, "interpreter belongs to another process");
        return nullptr;
    }

    return self_res;
}

void NifResTfLiteTensor::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResTfLiteTensor *)args;
    if (res) {
        // Out of the registry first, and val cleared while still holding the
        // registry lock. revalidate_tensors reads val for every entry it walks,
        // under that same lock, so clearing it beforehand would be one thread
        // writing what another is reading. The pointer is only ever dropped and
        // never deleted: it belongs to the interpreter's arena.
        if (res->interpreter && res->interpreter->tensors) {
            MutexLock registry(res->interpreter->tensors_lock);
            auto & list = *res->interpreter->tensors;
            for (auto it = list.begin(); it != list.end(); ++it) {
                if (*it == res) {
                    list.erase(it);
                    break;
                }
            }
            res->val = nullptr;
        } else {
            res->val = nullptr;
        }

        if (res->interpreter) {
            enif_release_resource(res->interpreter);
            res->interpreter = nullptr;
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

#ifdef TFLITE_BEAM_LITERT_API_ENABLED

NifResLiteRtEnvironment * NifResLiteRtEnvironment::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    auto res = (NifResLiteRtEnvironment *)enif_alloc_resource(
        NifResLiteRtEnvironment::type, sizeof(NifResLiteRtEnvironment));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResLiteRtEnvironment resource");
        return res;
    }
    res->val = nullptr;
    return res;
}

NifResLiteRtEnvironment * NifResLiteRtEnvironment::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResLiteRtEnvironment * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResLiteRtEnvironment::type, (void **)&self_res) ||
        self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResLiteRtEnvironment resource");
        return nullptr;
    }
    return self_res;
}

void NifResLiteRtEnvironment::destruct_resource(ErlNifEnv *, void *args) {
    auto res = (NifResLiteRtEnvironment *)args;
    if (res && res->val) {
        LiteRtDestroyEnvironment(res->val);
        res->val = nullptr;
    }
}

NifResLiteRtCompiledModel * NifResLiteRtCompiledModel::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    auto res = (NifResLiteRtCompiledModel *)enif_alloc_resource(
        NifResLiteRtCompiledModel::type, sizeof(NifResLiteRtCompiledModel));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResLiteRtCompiledModel resource");
        return res;
    }
    res->val = nullptr;
    res->model = nullptr;
    res->options = nullptr;
    res->environment = nullptr;
    res->inputs = nullptr;
    res->outputs = nullptr;
    res->input_sizes = nullptr;
    res->output_sizes = nullptr;
    res->signature = 0;
    res->profiler = nullptr;
    res->is_controlled = false;
    res->lock = enif_mutex_create(const_cast<char *>("litert_compiled_model"));
    if (res->lock == nullptr) {
        error = erlang::nif::error(env, "cannot create the compiled model lock");
        enif_release_resource(res);
        return nullptr;
    }
    return res;
}

// Same rule as caller_may_use for an interpreter: unclaimed is open to all, a
// claim binds the model to one process, and a claim whose process has died is
// released rather than stranding the model. Called with the resource lock held,
// so the flag and the pid are read together rather than one at a time.
bool compiled_model_caller_may_use(ErlNifEnv * env, NifResLiteRtCompiledModel * res) {
    if (res == nullptr || !res->is_controlled) return true;

    ErlNifPid caller;
    if (enif_self(env, &caller) == nullptr) return false;
    if (enif_compare_pids(&caller, &res->controlling_process) == 0) return true;

    if (enif_is_process_alive(env, &res->controlling_process)) return false;

    res->is_controlled = false;
    return true;
}

// Only resolves the handle. The ownership check needs the lock, and the lock
// has to stay held for the whole operation, so both live in CompiledModelUse.
NifResLiteRtCompiledModel * NifResLiteRtCompiledModel::get_resource(ErlNifEnv * env, ERL_NIF_TERM term, ERL_NIF_TERM &error) {
    NifResLiteRtCompiledModel * self_res = nullptr;
    if (!enif_get_resource(env, term, NifResLiteRtCompiledModel::type, (void **)&self_res) ||
        self_res == nullptr || self_res->val == nullptr) {
        error = erlang::nif::error(env, "cannot access NifResLiteRtCompiledModel resource");
        return nullptr;
    }
    return self_res;
}

void NifResLiteRtCompiledModel::destruct_resource(ErlNifEnv *, void *args) {
    auto res = (NifResLiteRtCompiledModel *)args;
    if (!res) return;

    // buffers first: they name the compiled model's tensors, so they cannot
    // outlive it, and the host memory behind them is released by the
    // deallocator LiteRtCreateTensorBufferFromHostMemory was given
    auto destroy = [](std::vector<LiteRtTensorBuffer> * bufs) {
        if (!bufs) return;
        for (auto b : *bufs) if (b) LiteRtDestroyTensorBuffer(b);
        delete bufs;
    };
    destroy(res->inputs);
    destroy(res->outputs);
    res->inputs = nullptr;
    res->outputs = nullptr;

    // the host memory behind the buffers is not freed here: it was handed to
    // LiteRT with the deallocator that frees it, and destroying the buffer above
    // is what calls it
    delete res->input_sizes;  res->input_sizes = nullptr;
    delete res->output_sizes; res->output_sizes = nullptr;

    // borrowed from the compiled model, so it is dropped rather than destroyed
    res->profiler = nullptr;

    if (res->val)     { LiteRtDestroyCompiledModel(res->val); res->val = nullptr; }
    if (res->options) { LiteRtDestroyOptions(res->options);   res->options = nullptr; }
    if (res->model)   { LiteRtDestroyModel(res->model);       res->model = nullptr; }

    if (res->environment) {
        enif_release_resource(res->environment);
        res->environment = nullptr;
    }

    // last, because everything above ran without contention only by virtue of
    // this being the final reference
    if (res->lock) {
        enif_mutex_destroy(res->lock);
        res->lock = nullptr;
    }
}

#endif  // TFLITE_BEAM_LITERT_API_ENABLED
