#include <vector>
#include <string.h>

#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"
#include "../helper.h"

#include "coral/tflite_utils.h"
#include "tflite/interpreter.h"
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
        edgetpu_free_devices(edgetpu_devices);
        return enif_make_list(env, 0, nullptr);
    }

    ERL_NIF_TERM * arr = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * num_devices);
    if (!arr) {
        edgetpu_free_devices(edgetpu_devices);
        return erlang::nif::error(env, "enif_alloc failed");
    }

    // Copied straight out of device.path. The scratch buffer this used to go
    // through was sized EDGETPU_DEVICE_NAME_BUFFER_SIZE while the length came
    // from snprintf, which reports what it would have written rather than what
    // it did, so a longer path read off the end of the buffer.
    for (size_t i = 0; i < num_devices; ++i) {
        const struct edgetpu_device& device = edgetpu_devices[i];
        const char * path = device.path ? device.path : "";
        size_t len = strlen(path);

        unsigned char * device_name_buf = enif_make_new_binary(env, len, &arr[i]);
        if (device_name_buf == nullptr) {
            edgetpu_free_devices(edgetpu_devices);
            enif_free(arr);
            return erlang::nif::error(env, "out of memory");
        }

        memcpy(device_name_buf, path, len);
    }

    ERL_NIF_TERM devices = enif_make_list_from_array(env, arr, (unsigned)num_devices);
    edgetpu_free_devices(edgetpu_devices);
    enif_free(arr);
    return devices;
}

ERL_NIF_TERM coral_get_edgetpu_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    std::string device;
    if (!erlang::nif::get(env, argv[0], device)) {
        return erlang::nif::error(env, "invalid device name");
    }

    NifResEdgeTpuContext * res = nullptr;
    ERL_NIF_TERM ret;

    auto c = coral::GetEdgeTpuContext(device);
    if (c.get() == nullptr) {
        return erlang::nif::error(env, "cannot find any available TPU");
    }

    if (!(res = NifResEdgeTpuContext::allocate_resource(env, ret))) {
        return ret;
    }
    ResourceRef<NifResEdgeTpuContext> hold(res);

    // hold a share of the context: asking for the same device twice hands back the
    // same one, and the device is only released once every share is gone
    res->context = new std::shared_ptr<edgetpu::EdgeTpuContext>(c);
    res->val = c.get();

    ret = enif_make_resource(env, res);
    return erlang::nif::ok(env, ret);
}

ERL_NIF_TERM coral_make_edgetpu_interpreter(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    ERL_NIF_TERM model_term = argv[0];
    ERL_NIF_TERM context_term = argv[1];
    NifResFlatBufferModel * model_res;
    NifResEdgeTpuContext * context_res;
    NifResInterpreter * interpreter_res = nullptr;

    ERL_NIF_TERM ret;

    if (!enif_get_resource(env, model_term, NifResFlatBufferModel::type, (void **)&model_res) || model_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResFlatBufferModel resource");
    }

    if (!enif_get_resource(env, context_term, NifResEdgeTpuContext::type, (void **)&context_res) || context_res->val == nullptr) {
        return erlang::nif::error(env, "cannot access NifResEdgeTpuContext resource");
    }

    if (!(interpreter_res = NifResInterpreter::allocate_resource(env, ret))) {
        return ret;
    }

    ResourceRef<NifResInterpreter> hold(interpreter_res);

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
    enif_keep_resource(model_res);

    // the interpreter delegates to this context, so it has to outlive the term it came in as
    interpreter_res->edgetpu_context = context_res;
    enif_keep_resource(context_res);

    ret = enif_make_resource(env, interpreter_res);
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

    // reads a tensor out of the interpreter and copies from it, so a rebuild
    // must not be replacing that interpreter while it does
    TFLITE_BEAM_INTERPRETER_IN_USE(interpreter_res);

    int64_t tensor_index;
    if (!erlang::nif::get(env, tensor_index_term, &tensor_index)) {
        return erlang::nif::error(env, "cannot get value of parameter 'tensor_index' in nif");
    }

    // An atom, which is what tflite_beam_coral:map_type/1 produces. The string
    // reader used here before takes charlists and binaries and refuses an atom,
    // so this failed for every type it was ever asked for, including the default.
    std::string type;
    if (!erlang::nif::get_atom(env, as_type_term, type)) {
        return erlang::nif::error(env,
            "expecting `type` to be one of the atoms nil, u8, u16, u32, u64, "
            "s8, s16, s32, s64, f32 or f64");
    }

    auto interpreter = interpreter_res->val;
    // interpreter->tensor takes an int, so a larger index used to narrow rather
    // than fall out of range: 4294967296 became 0 and answered with tensor zero.
    if (tensor_index < 0 || tensor_index >= interpreter->tensors_size()) {
        return erlang::nif::error(env, "tensor_index out-of-bound");
    }

    const TfLiteTensor * tensor = interpreter->tensor((int)tensor_index);
    if (tensor == nullptr) {
        return erlang::nif::error(env, "tensor_index out-of-bound");
    }

    // Two things have to hold before coral::DequantizeTensor is called at all.
    //
    // It reads the input through TensorData<uint8_t> or TensorData<int8_t> and
    // reaches LOG(FATAL) for anything else, which aborts the emulator rather
    // than returning. Asking for f32 from a float tensor did exactly that.
    if (tensor->type != kTfLiteUInt8 && tensor->type != kTfLiteInt8) {
        return erlang::nif::error(env,
            "only a uint8 or int8 tensor can be dequantized");
    }

    // And it reads the legacy scalar tensor.params, which TfLite leaves at zero
    // whenever the affine quantization does not have exactly one scale. A
    // per-axis tensor therefore came back as every value multiplied by a scale
    // of nought, which is a column of zeroes rather than an error.
    if (tensor->quantization.type == kTfLiteAffineQuantization) {
        const auto * affine =
            reinterpret_cast<const TfLiteAffineQuantization *>(tensor->quantization.params);
        if (affine == nullptr || affine->scale == nullptr || affine->scale->size != 1) {
            return erlang::nif::error(env,
                "this tensor is quantized per axis, which this call cannot undo; "
                "read its quantization_params and apply them yourself");
        }
    }

    ERL_NIF_TERM out;
    int ret_status;
    // The default produces real numbers. It used to produce the quantized type
    // back again, so scale * (q - zero_point) was truncated into an int8 or a
    // uint8, and since a scale is normally well below one that meant every value
    // came out as zero. Measured on a model with scale 0.0039: the true values
    // began 0.047, 0.059, 0.071 and this returned 0, 0, 0.
    if (type == "nil" || type == "f32") {
        auto vec = coral::DequantizeTensor<float>(*tensor);
        ret_status = erlang::nif::make(env, vec, out);
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
