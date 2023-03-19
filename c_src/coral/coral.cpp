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

class EdgeTpuContextDirect : public edgetpu::EdgeTpuContext {
 public:
  explicit EdgeTpuContextDirect(void * driver_wrapper);

  ~EdgeTpuContextDirect();

  const edgetpu::EdgeTpuManager::DeviceEnumerationRecord& GetDeviceEnumRecord()
      const final;

  edgetpu::EdgeTpuManager::DeviceOptions GetDeviceOptions() const final;

  bool IsReady() const final;

  void* GetDriverWrapper() const;

  void * driver_wrapper_{nullptr};
};

#define EDGETPU_DEVICE_NAME_BUFFER_SIZE 64

std::map<std::string, std::shared_ptr<edgetpu::EdgeTpuContext>> managedContext;

void destruct_egdetpu_context(ErlNifEnv *env, void *args) {
    auto res = (NifResEdgeTpuContext *)args;
    if (res->val) {
        res->val = nullptr;
    }
}

ERL_NIF_TERM coral_contains_edgetpu_custom_op(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);

    ERL_NIF_TERM self_nif = argv[0];
    NifResFlatBufferModel * self_res;
    if (!enif_get_resource(env, self_nif, NifResFlatBufferModel::type, (void **)&self_res) || self_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResFlatBufferModel resource");
    }

    if (coral::ContainsEdgeTpuCustomOp(*self_res->val)) {
        return erlang::nif::atom(env, "true");
    } else {
        return erlang::nif::atom(env, "false");
    }
}

ERL_NIF_TERM coral_edgetpu_devices(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    size_t num_devices;
    struct edgetpu_device * edgetpu_devices = edgetpu_list_devices(&num_devices);

    if (num_devices == 0) {
        return enif_make_list(env, 0, nullptr);
    }

    ERL_NIF_TERM * arr = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * num_devices);
    if (!arr) {
        edgetpu_free_devices(edgetpu_devices);
        return erlang::nif::error(env, "enif_alloc failed");
    }

    char * device_name = (char *)enif_alloc(sizeof(char) * EDGETPU_DEVICE_NAME_BUFFER_SIZE);
    if (!device_name) {
        edgetpu_free_devices(edgetpu_devices);
        enif_free(arr);
        return erlang::nif::error(env, "enif_alloc failed");
    }

    for (size_t i = 0; i < num_devices; ++i) {
        memset(device_name, 0, EDGETPU_DEVICE_NAME_BUFFER_SIZE);
        const struct edgetpu_device& device = edgetpu_devices[i];

        int len = snprintf(device_name, EDGETPU_DEVICE_NAME_BUFFER_SIZE, "%s", device.path);
        void * device_name_buf = nullptr;
        
        if (!(device_name_buf = enif_make_new_binary(env, len, &arr[i]))) {
            edgetpu_free_devices(edgetpu_devices);
            enif_free(arr);
            enif_free(device_name);
            return erlang::nif::error(env, "out of memory");
        }

        memcpy((char *)device_name_buf, device_name, len);
    }

    ERL_NIF_TERM devices = enif_make_list_from_array(env, arr, (unsigned)num_devices);
    edgetpu_free_devices(edgetpu_devices);
    enif_free(arr);
    enif_free(device_name);
    return devices;
}

ERL_NIF_TERM coral_get_edgetpu_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    std::string device;
    if (!erlang::nif::get(env, argv[0], device)) {
        return erlang::nif::error(env, "invalid device name");
    }

    NifResEdgeTpuContext * res = nullptr;

    auto c = coral::GetEdgeTpuContext(device);
    if (c.get() == nullptr) {
        return erlang::nif::error(env, "cannot find any available TPU");
    }

    if (!(res = alloc_resource_NifResEdgeTpuContext())) {
        return erlang::nif::error(env, "cannot allocate NifResEdgeTpuContext resource");
    }

    res->val = c.get();
    const edgetpu::EdgeTpuManager::DeviceEnumerationRecord& record = c->GetDeviceEnumRecord();
    managedContext[record.path] = c;

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    // todo: should we keep it?
    enif_keep_resource(res);
    return erlang::nif::ok(env, ret);
}

ERL_NIF_TERM coral_make_edgetpu_interpreter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM model_term = argv[0];
    ERL_NIF_TERM context_term = argv[1];
    NifResFlatBufferModel * model_res;
    NifResEdgeTpuContext * context_res;
    NifResInterpreter * interpreter_res = nullptr;

    if (!enif_get_resource(env, model_term, NifResFlatBufferModel::type, (void **)&model_res) || model_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResFlatBufferModel resource");
    }

    if (!enif_get_resource(env, context_term, NifResEdgeTpuContext::type, (void **)&context_res) || context_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResEdgeTpuContext resource");
    }

    interpreter_res = alloc_resource_NifResInterpreter();
    if (interpreter_res == nullptr) {
        return erlang::nif::error(env, "cannot allocate memory for interpreter resource");
    }

    tflite::FlatBufferModel * model = model_res->val;
    edgetpu::EdgeTpuContext * context = context_res->val;
    std::unique_ptr<tflite::Interpreter> interpreter;
    
    auto status = coral::MakeEdgeTpuInterpreter(*model, context, nullptr, nullptr, &interpreter);
    if (status != absl::OkStatus()) {
        return erlang::nif::error(env, "cannot make edgetpu interpreter");
    }

    if (interpreter->AllocateTensors() != kTfLiteOk) {
        return erlang::nif::error(env, "failed to allocate tensors");
    }

    interpreter_res->val = interpreter.release();
    interpreter_res->flatbuffer_model = model_res;
    interpreter_res->flatbuffer_model->reference_count++;

    ERL_NIF_TERM ret = enif_make_resource(env, interpreter_res);
    enif_release_resource(interpreter_res);
    return erlang::nif::ok(env, ret);
}

ERL_NIF_TERM coral_dequantize_tensor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    ERL_NIF_TERM interpreter_term = argv[0];
    ERL_NIF_TERM tensor_index_term = argv[1];
    ERL_NIF_TERM as_type_term = argv[2];

    NifResInterpreter * interpreter_res;

    if (!enif_get_resource(env, interpreter_term, NifResInterpreter::type, (void **)&interpreter_res) || interpreter_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResInterpreter resource");
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
    const TfLiteTensor * tensor = interpreter->tensor(tensor_index);
    if (tensor == nullptr) {
        return erlang::nif::error(env, "tensor_index out-of-bound");
    }

    ERL_NIF_TERM out;
    int ret_status;
    if (type == "nil") {
        if (tensor->type == kTfLiteUInt8) {
            auto vec = coral::DequantizeTensor<uint8_t>(*tensor);
            ret_status = erlang::nif::make(env, vec, out);
        } else if (tensor->type == kTfLiteInt8) {
            auto vec = coral::DequantizeTensor<int8_t>(*tensor);
            ret_status = erlang::nif::make(env, vec, out);
        } else {
            return erlang::nif::error(env, "only support tensor with its data type as 'uint8_t' or 'int8_t'");
        }
    } else if (type == "u8") {
        auto vec = coral::DequantizeTensor<uint8_t>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "u16") {
        auto vec = coral::DequantizeTensor<uint16_t>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "u32") {
        auto vec = coral::DequantizeTensor<uint32_t>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "u64") {
        auto vec = coral::DequantizeTensor<uint64_t>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "s8") {
        auto vec = coral::DequantizeTensor<int8_t>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "s16") {
        auto vec = coral::DequantizeTensor<int16_t>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "s32") {
        auto vec = coral::DequantizeTensor<int32_t>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "s64") {
        auto vec = coral::DequantizeTensor<int64_t>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "f32") {
        auto vec = coral::DequantizeTensor<float>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else if (type == "f64") {
        auto vec = coral::DequantizeTensor<double>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
    } else {
        return erlang::nif::error(env, "invalid value for parameter 'type' in nif");
    }

    if (ret_status != 0) {
        return erlang::nif::error(env, "cannot dequantize tensor");
    }

    return erlang::nif::ok(env, out);
}
