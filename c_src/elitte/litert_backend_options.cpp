#ifdef TFLITE_BEAM_LITERT_API_ENABLED
#include "litert_backend_options.h"

#include <cstring>
#include <initializer_list>
#include <limits>
#include <memory>
#include <stdexcept>

#include "../nif_utils.hpp"
#include "litert/c/options/litert_arm_options.h"
#include "litert/c/options/litert_cpu_options.h"
#include "litert/c/options/litert_google_tensor_options.h"
#include "litert/c/options/litert_gpu_options.h"
#include "litert/c/options/litert_intel_openvino_options.h"
#include "litert/c/options/litert_mediatek_options.h"
#include "litert/c/options/litert_qualcomm_options.h"

namespace {

void check(LiteRtStatus status) {
    if (status != kLiteRtStatusOk) throw std::runtime_error(LiteRtGetStatusString(status));
}

std::string name(ErlNifEnv *env, ERL_NIF_TERM term) {
    char value[80];
    if (!enif_get_atom(env, term, value, sizeof(value), ERL_NIF_LATIN1)) {
        throw std::invalid_argument("backend and option names must be atoms");
    }
    return value;
}

bool boolean(ErlNifEnv *env, ERL_NIF_TERM term) {
    if (enif_is_identical(term, erlang::nif::atom(env, "true"))) return true;
    if (enif_is_identical(term, erlang::nif::atom(env, "false"))) return false;
    throw std::invalid_argument("expecting a boolean backend option");
}

int64_t integer(ErlNifEnv *env, ERL_NIF_TERM term, int64_t minimum, int64_t maximum) {
    ErlNifSInt64 value;
    if (!enif_get_int64(env, term, &value) || value < minimum || value > maximum) {
        throw std::invalid_argument("backend option integer is out of range");
    }
    return value;
}

int choice(ErlNifEnv *env, ERL_NIF_TERM term, std::initializer_list<const char *> names) {
    int index = 0;
    for (const char *value : names) {
        if (enif_is_identical(term, erlang::nif::atom(env, value))) return index;
        ++index;
    }
    throw std::invalid_argument("unknown backend option enum value");
}

std::string string(ErlNifEnv *env, ERL_NIF_TERM term) {
    ErlNifBinary bytes;
    if (!enif_inspect_binary(env, term, &bytes)) throw std::invalid_argument("expecting a binary backend option");
    std::string value;
    if (bytes.size) value.assign(reinterpret_cast<const char *>(bytes.data), bytes.size);
    for (unsigned char c : value) {
        if (c < 32 || c == 127 || c == '"' || c == '\\') {
            throw std::invalid_argument("backend string cannot contain controls, quotes or backslashes");
        }
    }
    return value;
}

class MapIterator {
public:
    MapIterator(ErlNifEnv *env, ERL_NIF_TERM term) : env_(env) {
        if (!enif_map_iterator_create(env, term, &iterator_, ERL_NIF_MAP_ITERATOR_FIRST)) {
            throw std::invalid_argument("expecting a backend options map");
        }
    }
    ~MapIterator() { enif_map_iterator_destroy(env_, &iterator_); }
    bool pair(ERL_NIF_TERM &key, ERL_NIF_TERM &value) {
        return enif_map_iterator_get_pair(env_, &iterator_, &key, &value);
    }
    void next() { enif_map_iterator_next(env_, &iterator_); }
private:
    ErlNifEnv *env_;
    ErlNifMapIterator iterator_;
};

template<class Fn>
void each(ErlNifEnv *env, ERL_NIF_TERM map, Fn fn) {
    MapIterator iterator(env, map);
    ERL_NIF_TERM key, value;
    while (iterator.pair(key, value)) {
        fn(name(env, key), value);
        iterator.next();
    }
}

template<class T, class Create, class Destroy>
auto create(Create fn, Destroy destroy) {
    T *value = nullptr;
    LiteRtStatus status = fn(&value);
    std::unique_ptr<T, Destroy> result(value, destroy);
    check(status);
    return result;
}

template<class T, class Serialize>
std::pair<std::string, std::string> payload(T *options, Serialize fn) {
    const char *identifier = nullptr;
    void *data = nullptr;
    void (*destroy)(void *) = nullptr;
    LiteRtStatus status = fn(options, &identifier, &data, &destroy);
    if (status != kLiteRtStatusOk) {
        if (data && destroy) destroy(data);
        check(status);
    }
    if (!destroy) throw std::runtime_error("backend returned no payload deleter");
    std::unique_ptr<void, decltype(destroy)> hold(data, destroy);
    if (!identifier || !data) throw std::runtime_error("backend returned an invalid options payload");
    return {identifier, static_cast<const char *>(data)};
}

void unknown(const std::string &key) {
    throw std::invalid_argument("unknown backend option: " + key);
}

std::pair<std::string, std::string> cpu(ErlNifEnv *env, ERL_NIF_TERM map) {
    auto options = create<LrtCpuOptions>(LrtCreateCpuOptions, LrtDestroyCpuOptions);
    bool path = false, descriptor = false;
    each(env, map, [&](const std::string &key, ERL_NIF_TERM value) {
        if (key == "num_threads") {
            int threads = integer(env, value, -1, std::numeric_limits<int>::max());
            if (threads == 0) throw std::invalid_argument("num_threads must be -1 or positive");
            check(LrtSetCpuOptionsNumThread(options.get(), threads));
        } else if (key == "kernel_mode") {
            check(LrtSetCpuOptionsKernelMode(options.get(), static_cast<LiteRtCpuKernelMode>(
                choice(env, value, {"delegate", "reference", "builtin"}))));
        } else if (key == "enable_ynnpack") {
            check(LrtSetCpuOptionsEnableYNNPack(options.get(), boolean(env, value)));
        } else if (key == "xnnpack_flags") {
            check(LrtSetCpuOptionsXNNPackFlags(options.get(), integer(env, value, 0, UINT32_MAX)));
        } else if (key == "hint_fully_delegated_to_single_delegate") {
            check(LrtSetCpuOptionsHintFullyDelegatedToSingleDelegate(options.get(), boolean(env, value)));
        } else if (key == "weight_cache_path") {
            path = true;
            check(LrtSetCpuOptionsXnnPackWeightCachePath(options.get(), string(env, value).c_str()));
        } else if (key == "weight_cache_fd") {
            descriptor = true;
            check(LrtSetCpuOptionsXnnPackWeightCacheFileDescriptor(options.get(),
                integer(env, value, 0, std::numeric_limits<int>::max())));
        } else unknown(key);
    });
    if (path && descriptor) throw std::invalid_argument("weight_cache_path and weight_cache_fd are mutually exclusive");
    return payload(options.get(), LrtGetOpaqueCpuOptionsData);
}

std::pair<std::string, std::string> gpu(ErlNifEnv *env, ERL_NIF_TERM map, int precision) {
    auto options = create<LrtGpuOptions>(LrtCreateGpuOptions, LrtDestroyGpuOptions);
    if (precision) check(LrtSetGpuAcceleratorCompilationOptionsPrecision(options.get(),
        static_cast<LiteRtDelegatePrecision>(precision)));
    each(env, map, [&](const std::string &key, ERL_NIF_TERM value) {
        if (key == "constant_tensors_sharing") {
            check(LrtSetGpuOptionsConstantTensorsSharing(options.get(), boolean(env, value)));
        } else if (key == "use_ir_model") {
            check(LrtSetGpuOptionsUseIrModel(options.get(), boolean(env, value)));
        } else if (key == "use_metal_argument_buffers") {
            check(LrtSetGpuOptionsUseMetalArgumentBuffers(options.get(), boolean(env, value)));
        } else if (key == "metal_residency_set") {
            check(LrtSetGpuOptionsMetalResidencySet(options.get(), boolean(env, value)));
        } else if (key == "hint_fully_delegated_to_single_delegate") {
            check(LrtSetGpuOptionsHintFullyDelegatedToSingleDelegate(options.get(), boolean(env, value)));
        } else unknown(key);
    });
    return payload(options.get(), LrtGetOpaqueGpuOptionsData);
}

std::pair<std::string, std::string> arm(ErlNifEnv *env, ERL_NIF_TERM map) {
    auto options = create<LrtArmOptionsT>(LrtCreateArmOptions, LrtDestroyArmOptions);
    each(env, map, [&](const std::string &key, ERL_NIF_TERM value) {
        if (key == "enable_just_in_time") check(LrtArmOptionsSetEnableJustInTime(options.get(), boolean(env, value)));
        else unknown(key);
    });
    return payload(options.get(), LrtGetOpaqueArmOptionsData);
}

std::pair<std::string, std::string> intel(ErlNifEnv *env, ERL_NIF_TERM map) {
    auto options = create<LrtIntelOpenVinoOptionsT>(LrtIntelOpenVinoOptionsCreate, LrtDestroyIntelOpenVinoOptions);
    each(env, map, [&](const std::string &key, ERL_NIF_TERM value) {
        if (key == "enable_weight_sharing") {
            check(LrtIntelOpenVinoOptionsSetEnableWeightSharing(options.get(), boolean(env, value)));
        } else if (key == "performance_mode") {
            check(LrtIntelOpenVinoOptionsSetPerformanceMode(options.get(), static_cast<LiteRtIntelOpenVinoPerformanceMode>(
                choice(env, value, {"latency", "throughput", "cumulative_throughput"}))));
        } else unknown(key);
    });
    return payload(options.get(), LrtGetOpaqueIntelOpenVinoOptionsData);
}

std::pair<std::string, std::string> mediatek(ErlNifEnv *env, ERL_NIF_TERM map) {
    auto options = create<LrtMediatekOptions>(LrtCreateMediatekOptions, LrtDestroyMediatekOptions);
    each(env, map, [&](const std::string &key, ERL_NIF_TERM value) {
        if (key == "gemma_compiler_optimizations") {
            check(LrtSetMediatekOptionsGemmaCompilerOptimizations(options.get(), boolean(env, value)));
        } else if (key == "option_bundle") {
            check(LrtSetMediatekOptionsOptionBundle(options.get(), string(env, value).c_str()));
        } else if (key == "option_bundle_decode") {
            check(LrtSetMediatekOptionsOptionBundleDecode(options.get(), string(env, value).c_str()));
        } else if (key == "option_bundle_prefill") {
            check(LrtSetMediatekOptionsOptionBundlePrefill(options.get(), string(env, value).c_str()));
        } else if (key == "use_get_supported_operations") {
            check(LrtSetMediatekOptionsUseGetSupportedOperations(options.get(), boolean(env, value)));
        } else unknown(key);
    });
    return payload(options.get(), LrtGetOpaqueMediatekOptionsData);
}

void coherency(ErlNifEnv *env, LrtGoogleTensorOptions options, ERL_NIF_TERM list, bool input) {
    unsigned count;
    if (!enif_get_list_length(env, list, &count)) throw std::invalid_argument("expecting a coherency list");
    ERL_NIF_TERM head, tail = list;
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        int arity;
        const ERL_NIF_TERM *entry;
        if (!enif_get_tuple(env, head, &arity, &entry) || arity != 3) {
            throw std::invalid_argument("coherency entries must be {signature, tensor, boolean}");
        }
        std::string signature = string(env, entry[0]), tensor = string(env, entry[1]);
        if (signature.find_first_of(":= ") != std::string::npos || tensor.find_first_of(":= ") != std::string::npos) {
            throw std::invalid_argument("coherency names cannot contain colon, equals or spaces");
        }
        bool prefer = boolean(env, entry[2]);
        check(input ? LrtGoogleTensorOptionsSetInputCoherency(options, signature.c_str(), tensor.c_str(), prefer)
                    : LrtGoogleTensorOptionsSetOutputCoherency(options, signature.c_str(), tensor.c_str(), prefer));
    }
}

std::pair<std::string, std::string> google(ErlNifEnv *env, ERL_NIF_TERM map) {
    auto options = create<LrtGoogleTensorOptionsT>(LrtCreateGoogleTensorOptions, LrtDestroyGoogleTensorOptions);
    each(env, map, [&](const std::string &key, ERL_NIF_TERM value) {
        if (key == "input_coherency") coherency(env, options.get(), value, true);
        else if (key == "output_coherency") coherency(env, options.get(), value, false);
        else unknown(key);
    });
    return payload(options.get(), LrtGetOpaqueGoogleTensorOptionsData);
}

std::pair<std::string, std::string> qualcomm(ErlNifEnv *env, ERL_NIF_TERM map) {
    auto options = create<LrtQualcommOptionsT>(LrtCreateQualcommOptions, LrtDestroyQualcommOptions);
    each(env, map, [&](const std::string &key, ERL_NIF_TERM value) {
        if (key == "enable_weight_sharing") {
            check(LrtQualcommOptionsSetEnableWeightSharing(options.get(), boolean(env, value)));
        } else if (key == "htp_pd_session") {
            check(LrtQualcommOptionsSetHtpPdSession(options.get(), static_cast<LrtQualcommOptionsHtpPdSession>(
                choice(env, value, {"unsigned", "signed", "adaptive"}))));
        } else if (key == "dsp_pd_session") {
            check(LrtQualcommOptionsSetDspPdSession(options.get(), static_cast<LrtQualcommOptionsDspPdSession>(
                choice(env, value, {"unsigned", "signed", "adaptive"}))));
        } else if (key == "dsp_encoding") {
            check(LrtQualcommOptionsSetDspEncoding(options.get(), static_cast<LrtQualcommOptionsDspEncoding>(
                choice(env, value, {"static", "dynamic"}))));
        } else if (key == "dsp_performance_mode") {
            check(LrtQualcommOptionsSetDspPerformanceMode(options.get(), static_cast<LrtQualcommOptionsDspPerformanceMode>(
                choice(env, value, {"default", "sustained_high_performance", "burst", "high_performance",
                    "power_saver", "low_power_saver", "high_power_saver", "low_balanced", "balanced"}))));
        } else if (key == "backend") {
            check(LrtQualcommOptionsSetBackend(options.get(), static_cast<LrtQualcommOptionsBackend>(
                choice(env, value, {"undefined", "gpu", "htp", "dsp", "ir", "lpai"}))));
        } else if (key == "lpai_target") {
            check(LrtQualcommOptionsSetLpaiTarget(options.get(), static_cast<LrtQualcommOptionsLpaiTarget>(
                choice(env, value, {"x86", "arm", "adsp", "tensilica"}))));
        } else if (key == "lpai_fps") {
            check(LrtQualcommOptionsSetLpaiFps(options.get(), integer(env, value, 1, UINT32_MAX)));
        } else if (key == "lpai_ftrt_ratio") {
            check(LrtQualcommOptionsSetLpaiFtrtRatio(options.get(), integer(env, value, 1, UINT32_MAX)));
        } else if (key == "lpai_client_perf_type") {
            check(LrtQualcommOptionsSetLpaiClientPerfType(options.get(), static_cast<LrtQualcommOptionsLpaiClientPerfType>(
                choice(env, value, {"default", "real_time", "non_real_time"}))));
        } else if (key == "lpai_core_affinity") {
            check(LrtQualcommOptionsSetLpaiCoreAffinityType(options.get(), static_cast<LrtQualcommOptionsLpaiCoreAffinityType>(
                choice(env, value, {"default", "soft", "hard"}))));
        } else if (key == "lpai_core_selection") {
            check(LrtQualcommOptionsSetLpaiCoreSelection(options.get(), integer(env, value, 0, UINT32_MAX)));
        } else if (key == "qnn_lib_dir") {
            check(LrtQualcommOptionsSetQnnLibDir(options.get(), string(env, value).c_str()));
        } else if (key == "dsp_skel_dir") {
            check(LrtQualcommOptionsSetDspSkelDir(options.get(), string(env, value).c_str()));
        } else if (key == "graph_transform") {
            check(LrtQualcommOptionsSetGraphTransform(options.get(), string(env, value).c_str()));
        } else unknown(key);
    });
    return payload(options.get(), LrtGetOpaqueQualcommOptionsData);
}

}  // namespace

LiteRtBackendPayloads litert_backend_payloads(ErlNifEnv *env, ERL_NIF_TERM options, int precision) {
    LiteRtBackendPayloads result;
    bool has_gpu = false;
    each(env, options, [&](const std::string &backend, ERL_NIF_TERM map) {
        if (backend == "cpu") result.push_back(cpu(env, map));
        else if (backend == "gpu") { result.push_back(gpu(env, map, precision)); has_gpu = true; }
        else if (backend == "arm") result.push_back(arm(env, map));
        else if (backend == "intel_openvino") result.push_back(intel(env, map));
        else if (backend == "mediatek") result.push_back(mediatek(env, map));
        else if (backend == "google_tensor") result.push_back(google(env, map));
        else if (backend == "qualcomm") result.push_back(qualcomm(env, map));
        else throw std::invalid_argument("unknown backend: " + backend);
    });
    if (precision && !has_gpu) result.push_back(gpu(env, enif_make_new_map(env), precision));
    return result;
}

ERL_NIF_TERM litert_backend_options(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 1) return enif_make_badarg(env);
    auto payloads = litert_backend_payloads(env, argv[0], 0);
    ERL_NIF_TERM result = enif_make_list(env, 0);
    for (size_t i = payloads.size(); i-- > 0;) {
        ERL_NIF_TERM terms[2];
        const std::string *values[] = {&payloads[i].first, &payloads[i].second};
        for (int j = 0; j < 2; ++j) {
            unsigned char *out = enif_make_new_binary(env, values[j]->size(), &terms[j]);
            if (!out && !values[j]->empty()) throw std::bad_alloc();
            if (!values[j]->empty()) memcpy(out, values[j]->data(), values[j]->size());
        }
        result = enif_make_list_cell(env, enif_make_tuple2(env, terms[0], terms[1]), result);
    }
    return erlang::nif::ok(env, result);
}
#endif
