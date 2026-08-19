#include <cstring>
#include <map>
#include <string>
#include <vector>

#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"

#include "delegate.h"

#ifdef TFLITE_BEAM_XNNPACK_ENABLED
#include "tensorflow/lite/delegates/xnnpack/xnnpack_delegate.h"
#endif

// What was compiled in, not what the machine has: whether a device is present is
// answered by trying to create the delegate and getting {error, _} back. The two
// questions have different answers on the same binary, so they get different
// functions.
ERL_NIF_TERM delegate_available(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 0) return enif_make_badarg(env);

    std::vector<ERL_NIF_TERM> available;

#ifdef TFLITE_BEAM_XNNPACK_ENABLED
    available.push_back(erlang::nif::atom(env, "xnnpack"));
#endif

    return enif_make_list_from_array(env, available.data(), (unsigned)available.size());
}

#ifdef TFLITE_BEAM_XNNPACK_ENABLED

static bool get_atom_list(ErlNifEnv *env, ERL_NIF_TERM list, std::vector<std::string> &out) {
    if (!enif_is_list(env, list)) return false;

    ERL_NIF_TERM head, tail = list;
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        std::string name;
        if (!erlang::nif::get_atom(env, head, name)) return false;
        out.push_back(name);
    }
    return true;
}

// Mapped by name, never by position: bit 0x100 is unassigned, so counting the
// macros in order gives the wrong values from SLOW_CONSISTENT_ARITHMETIC on.
static const std::map<std::string, uint32_t> & xnnpack_flags() {
    static const std::map<std::string, uint32_t> flags = {
        {"qs8", TFLITE_XNNPACK_DELEGATE_FLAG_QS8},
        {"qu8", TFLITE_XNNPACK_DELEGATE_FLAG_QU8},
        {"force_fp16", TFLITE_XNNPACK_DELEGATE_FLAG_FORCE_FP16},
        {"dynamic_fully_connected", TFLITE_XNNPACK_DELEGATE_FLAG_DYNAMIC_FULLY_CONNECTED},
        {"variable_operators", TFLITE_XNNPACK_DELEGATE_FLAG_VARIABLE_OPERATORS},
        {"transient_indirection_buffer", TFLITE_XNNPACK_DELEGATE_FLAG_TRANSIENT_INDIRECTION_BUFFER},
        {"enable_latest_operators", TFLITE_XNNPACK_DELEGATE_FLAG_ENABLE_LATEST_OPERATORS},
        {"enable_subgraph_reshaping", TFLITE_XNNPACK_DELEGATE_FLAG_ENABLE_SUBGRAPH_RESHAPING},
        {"slow_consistent_arithmetic", TFLITE_XNNPACK_DELEGATE_FLAG_SLOW_CONSISTENT_ARITHMETIC},
        {"disable_subgraph_reshaping", TFLITE_XNNPACK_DELEGATE_FLAG_DISABLE_SUBGRAPH_RESHAPING},
        {"disable_dynamically_quantized_ops", TFLITE_XNNPACK_DELEGATE_FLAG_DISABLE_DYNAMICALLY_QUANTIZED_OPS},
    };
    return flags;
}

ERL_NIF_TERM delegate_xnnpack_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);

    int num_threads;
    if (!erlang::nif::get(env, argv[0], &num_threads)) {
        return erlang::nif::error(env, "expecting num_threads to be an integer");
    }

    std::vector<std::string> flag_names;
    if (!get_atom_list(env, argv[1], flag_names)) {
        return erlang::nif::error(env, "expecting flags to be a list of atoms");
    }

    // added to the defaults rather than replacing them: TfLite expresses turning
    // a default off as its own flag, DISABLE_SUBGRAPH_RESHAPING and the like
    TfLiteXNNPackDelegateOptions options = TfLiteXNNPackDelegateOptionsDefault();
    for (auto & name : flag_names) {
        auto found = xnnpack_flags().find(name);
        if (found == xnnpack_flags().end()) {
            return erlang::nif::error(env, ("unknown xnnpack flag: " + name).c_str());
        }
        options.flags |= found->second;
    }
    options.num_threads = num_threads;

    NifResDelegate * res = nullptr;
    ERL_NIF_TERM ret;
    if (!(res = NifResDelegate::allocate_resource(env, ret))) {
        return ret;
    }

    // the resource has to own this: TfLiteXNNPackDelegateCreate keeps the pointer
    // it is handed, and only re-points it at its own copy once the cache is live
    std::string weight_cache_file_path;
    if (!erlang::nif::check_nil(env, argv[2])) {
        if (!erlang::nif::get(env, argv[2], weight_cache_file_path)) {
            enif_release_resource(res);
            return erlang::nif::error(env, "expecting weight_cache_file_path to be a string or nil");
        }

        res->owned_path = (char *)enif_alloc(weight_cache_file_path.size() + 1);
        if (res->owned_path == nullptr) {
            enif_release_resource(res);
            return erlang::nif::error(env, "cannot allocate memory for weight_cache_file_path");
        }
        memcpy(res->owned_path, weight_cache_file_path.c_str(), weight_cache_file_path.size() + 1);
        options.weight_cache_file_path = res->owned_path;
    }

    res->val = TfLiteXNNPackDelegateCreate(&options);
    if (res->val == nullptr) {
        enif_release_resource(res);
        return erlang::nif::error(env, "cannot create XNNPACK delegate");
    }
    res->deleter = TfLiteXNNPackDelegateDelete;

    ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return erlang::nif::ok(env, ret);
}

#endif
