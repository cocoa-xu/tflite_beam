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

    res->reference_count = 0;
    res->dropped_in_erlang = false;
    res->deleted = false;

    return res;
}

void NifResBuiltinOpResolver::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResBuiltinOpResolver *)args;
    if (res) {
        if (res->val) {
            res->dropped_in_erlang = true;
            if (!res->deleted && res->reference_count == 0) {
                delete res->val;
                res->val = nullptr;
            }
        }
    }
}

NifResErrorReporter * NifResErrorReporter::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResErrorReporter * res = (NifResErrorReporter *)enif_alloc_resource(NifResErrorReporter::type, sizeof(NifResErrorReporter));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResFlatBufferModel resource");
        return res;
    }

    return res;
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
    
    res->reference_count = 0;
    res->dropped_in_erlang = false;
    res->deleted = false;

    return res;
}

void NifResFlatBufferModel::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResFlatBufferModel *)args;
    if (res) {
        if (res->val) {
            res->dropped_in_erlang = true;
            if (!res->deleted && res->reference_count == 0) {
                delete res->val;
                res->val = nullptr;
            }
        }
    }
}

NifResInterpreterBuilder * NifResInterpreterBuilder::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResInterpreterBuilder * res = (NifResInterpreterBuilder *)enif_alloc_resource(NifResInterpreterBuilder::type, sizeof(NifResInterpreterBuilder));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResInterpreterBuilder resource");
        return res;
    }

    res->op_resolver = nullptr;
    res->flatbuffer_model = nullptr;

    return res;
}

void NifResInterpreterBuilder::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResInterpreterBuilder *)args;
    if (res) {
        if (res->val) {
            if (res->op_resolver) {
                res->op_resolver->reference_count--;

                if (res->op_resolver->reference_count == 0 && res->op_resolver->dropped_in_erlang) {
                    if (res->op_resolver->val) {
                        delete res->op_resolver->val;
                    }
                    res->op_resolver->val = nullptr;
                    res->op_resolver->deleted = true;
                    res->op_resolver = nullptr;
                }
            }

            if (res->flatbuffer_model) {
                res->flatbuffer_model->reference_count--;

                if (res->flatbuffer_model->reference_count == 0 && res->flatbuffer_model->dropped_in_erlang) {
                    if (res->flatbuffer_model->val) {
                        delete res->flatbuffer_model->val;
                    }
                    res->flatbuffer_model->val = nullptr;
                    res->flatbuffer_model->deleted = true;
                    enif_release_resource(res->flatbuffer_model);
                    res->flatbuffer_model = nullptr;
                }
            }

            delete res->val;
            res->val = nullptr;
        }
    }
}

NifResInterpreter * NifResInterpreter::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResInterpreter * res = (NifResInterpreter *)enif_alloc_resource(NifResInterpreter::type, sizeof(NifResInterpreter));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResInterpreter resource");
        return res;
    }

    res->flatbuffer_model = nullptr;
    return res;
}

void NifResInterpreter::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResInterpreter *)args;
    if (res) {
        if (res->val) {
            if (res->flatbuffer_model) {
                res->flatbuffer_model->reference_count--;

                if (res->flatbuffer_model->reference_count == 0 && res->flatbuffer_model->dropped_in_erlang) {
                    if (res->flatbuffer_model->val) {
                        delete res->flatbuffer_model->val;
                    }
                    res->flatbuffer_model->val = nullptr;
                    res->flatbuffer_model->deleted = true;
                    enif_release_resource(res->flatbuffer_model);
                    res->flatbuffer_model = nullptr;
                }
            }
            delete res->val;
            res->val = nullptr;
        }
    }
}

NifResTfLiteTensor * NifResTfLiteTensor::allocate_resource(ErlNifEnv * env, ERL_NIF_TERM &error) {
    NifResTfLiteTensor * res = (NifResTfLiteTensor *)enif_alloc_resource(NifResTfLiteTensor::type, sizeof(NifResTfLiteTensor));
    if (res == nullptr) {
        error = erlang::nif::error(env, "cannot allocate NifResTfLiteTensor resource");
        return res;
    }

    return res;
}

void NifResTfLiteTensor::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResTfLiteTensor *)args;
    if (res) {
        if (res->val) {
            if (!res->borrowed) {
                delete res->val;
            }
            res->val = nullptr;
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

    return res;
}

void NifResEdgeTpuContext::destruct_resource(ErlNifEnv *env, void *args) {
    auto res = (NifResEdgeTpuContext *)args;
    if (res->val) {
        res->val = nullptr;
    }
}

#endif
