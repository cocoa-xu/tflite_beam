#include <vector>
#include <string.h>

#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.hpp"
#include "../helper.h"

#include "coral/tflite_utils.h"
#include "tensorflow/lite/interpreter.h"
#include "tflite/public/edgetpu.h"
#include "tflite/public/edgetpu_c.h"

#define EDGETPU_DEVICE_NAME_BUFFER_SIZE 64

std::map<void *, std::shared_ptr<edgetpu::EdgeTpuContext>> managedContext;

using NifResEdgeTpuContext = erlang_nif_res<edgetpu::EdgeTpuContext *>;
using NifResFlatBufferModel = erlang_nif_res<tflite::FlatBufferModel *>;
using NifResInterpreter = erlang_nif_res<tflite::Interpreter *>;

void destruct_egdetpu_context(ErlNifEnv *env, void *args) {
    auto res = (NifResEdgeTpuContext *)args;
    if (res->val) {
        auto iter = managedContext.find(res->val);
        if (iter != managedContext.end()) {
            managedContext.erase(iter);
        }
        res->val = nullptr;
    }
}

ERL_NIF_TERM coral_contains_edgetpu_custom_op(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;
    if (enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res)) {
        if (self_res->val) {
            if (coral::ContainsEdgeTpuCustomOp(*self_res->val)) {
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

ERL_NIF_TERM coral_edgetpu_devices(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    size_t num_devices;
    std::unique_ptr<edgetpu_device, decltype(&edgetpu_free_devices)> devices(
            edgetpu_list_devices(&num_devices), &edgetpu_free_devices);

    if (num_devices > 0) {
        ERL_NIF_TERM * arr = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * num_devices);
        char * device_name = (char *)enif_alloc(sizeof(char) * EDGETPU_DEVICE_NAME_BUFFER_SIZE);
        for (size_t i = 0; i < num_devices; ++i) {
            memset(device_name, 0, EDGETPU_DEVICE_NAME_BUFFER_SIZE);
            const auto& device = devices.get()[i];
            int len = enif_snprintf(device_name, EDGETPU_DEVICE_NAME_BUFFER_SIZE-1, "%s", device.path);
            void * device_name_buf = nullptr;
            if ((device_name_buf = enif_make_new_binary(env, len, &arr[i])) != nullptr) {
                strncpy((char *)device_name_buf, device_name, len);
            } else {
                enif_free(arr);
                enif_free(device_name);
                return erlang::nif::error(env, "out of memory");
            }
        }
        ERL_NIF_TERM devices = enif_make_list_from_array(env, arr, (unsigned)num_devices);
        enif_free(arr);
        enif_free(device_name);
        return devices;
    } else {
        return enif_make_list(env, 0, nullptr);
    }
}

ERL_NIF_TERM coral_get_edgetpu_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    std::string device;
    if (erlang::nif::get(env, argv[0], device)) {
        NifResEdgeTpuContext * res;
        auto c = coral::GetEdgeTpuContext(device);
        if (c.get() != nullptr) {
            if (alloc_resource(&res)) {
                // take ownership
                edgetpu::EdgeTpuContext * context = c.get();
                res->val = context;
                managedContext[context] = c;
                ERL_NIF_TERM ret = enif_make_resource(env, res);
                enif_release_resource(res);
                return erlang::nif::ok(env, ret);
            } else {
                // free
                return erlang::nif::error(env, "cannot allocate memory for resource");
            }
        } else {
            return erlang::nif::error(env, "cannot find any available TPU");
        }
    } else {
        return erlang::nif::error(env, "invalid device name");
    }
}

ERL_NIF_TERM coral_make_edgetpu_interpreter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM model_term = argv[0];
    ERL_NIF_TERM context_term = argv[1];
    NifResFlatBufferModel * model_res;
    NifResEdgeTpuContext * context_res;
    NifResInterpreter * interpreter_res;

    if (!enif_get_resource(env, model_term, NifResFlatBufferModel::type, (void **)&model_res)) {
        return erlang::nif::error(env, "cannot access model resource");
    }
    if (!enif_get_resource(env, context_term, NifResEdgeTpuContext::type, (void **)&context_res)) {
        return erlang::nif::error(env, "cannot access context resource");
    }

    if (model_res->val == nullptr || context_res->val == nullptr) {
        return erlang::nif::error(env, "oh nyo erlang");
    }
    if (!alloc_resource(&interpreter_res)) {
        return erlang::nif::error(env, "cannot allocate memory for interpreter resource");
    }

    auto model = model_res->val;
    auto context = context_res->val;
    std::unique_ptr<tflite::Interpreter> tmp;

    auto status = coral::MakeEdgeTpuInterpreter(*model, context, nullptr, nullptr, &tmp);
    if (status == absl::OkStatus()) {
        interpreter_res->val = tmp.release();
        ERL_NIF_TERM ret = enif_make_resource(env, interpreter_res);
        enif_release_resource(interpreter_res);
        return erlang::nif::ok(env, ret);
    } else {
        return erlang::nif::error(env, "cannot make edgetpu interpreter");
    }
}

ERL_NIF_TERM coral_dequantize_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    ERL_NIF_TERM interpreter_term = argv[0];
    ERL_NIF_TERM tensor_index_term = argv[1];
    ERL_NIF_TERM as_type_term = argv[2];

    NifResInterpreter * interpreter_res;

    if (!enif_get_resource(env, interpreter_term, NifResInterpreter::type, (void **)&interpreter_res)) {
        return erlang::nif::error(env, "cannot access interpreter resource");
    }

    if (interpreter_res->val == nullptr) {
        return erlang::nif::error(env, "oh nyo erlang");
    }

    int64_t tensor_index;
    if (!erlang::nif::get(env, tensor_index_term, &tensor_index)) {
        return erlang::nif::error(env, "cannot get value of parameter 'tensor_index' in nif");
    }

    std::string type;
    if (!erlang::nif::get(env, as_type_term, type)) {
        return erlang::nif::error(env, "cannot get value of parameter 'type' in nif");
    }

    auto interpreter = interpreter_res->val;
    const auto& tensor = *interpreter->tensor(tensor_index);
    ERL_NIF_TERM out;
    int ret_status;
    if (type == "nil") {
        if (tensor.type == kTfLiteUInt8) {
            auto vec = coral::DequantizeTensor<uint8_t>(tensor);
            ret_status = erlang::nif::make(env, vec, out);
        } else if (tensor.type == kTfLiteInt8) {
            auto vec = coral::DequantizeTensor<int8_t>(tensor);
            ret_status = erlang::nif::make(env, vec, out);
        } else {
            return erlang::nif::error(env, "only support tensor with its data type as 'uint8_t' or 'int8_t'");
        }
    } else if (type == "u8") {
        auto vec = coral::DequantizeTensor<uint8_t>(tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "u16") {
        auto vec = coral::DequantizeTensor<uint16_t>(tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "u32") {
        auto vec = coral::DequantizeTensor<uint32_t>(tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "u64") {
        auto vec = coral::DequantizeTensor<uint64_t>(tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "s8") {
        auto vec = coral::DequantizeTensor<int8_t>(tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "s16") {
        auto vec = coral::DequantizeTensor<int16_t>(tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "s32") {
        auto vec = coral::DequantizeTensor<int32_t>(tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "s64") {
        auto vec = coral::DequantizeTensor<int64_t>(tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "f32") {
        auto vec = coral::DequantizeTensor<float>(tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "f64") {
        auto vec = coral::DequantizeTensor<double>(tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else {
        return erlang::nif::error(env, "invalid value for parameter 'type' in nif");
    }

    if (ret_status != 0) {
        return erlang::nif::error(env, "cannot dequantize tensor");
    }

    return erlang::nif::ok(env, out);
}
