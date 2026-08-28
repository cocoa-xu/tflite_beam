#include <cstring>
#include <map>
#include <string>
#include <vector>

#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "../erlang_nif_resource.h"

#include "delegate.h"

#include "tflite/shared_library.h"
#include "tflite/delegates/external/external_delegate_interface.h"

#ifdef TFLITE_BEAM_XNNPACK_ENABLED
#include "tflite/delegates/xnnpack/xnnpack_delegate.h"
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
    // loading a plugin needs nothing from TfLite's own build options, only the
    // dynamic loader, so this one is there on every target
    available.push_back(erlang::nif::atom(env, "external"));

    return enif_make_list_from_array(env, available.data(), (unsigned)available.size());
}

static bool get_string_pairs(ErlNifEnv *env, ERL_NIF_TERM list,
                             std::vector<std::string> &keys,
                             std::vector<std::string> &values) {
    if (!enif_is_list(env, list)) return false;

    ERL_NIF_TERM head, tail = list;
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        int arity;
        const ERL_NIF_TERM * pair;
        if (!enif_get_tuple(env, head, &arity, &pair) || arity != 2) return false;

        std::string key, value;
        if (!erlang::nif::get(env, pair[0], key)) return false;
        if (!erlang::nif::get(env, pair[1], value)) return false;
        keys.push_back(key);
        values.push_back(value);
    }
    return true;
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
    ResourceRef<NifResDelegate> hold(res);

    // the resource has to own this: TfLiteXNNPackDelegateCreate keeps the pointer
    // it is handed, and only re-points it at its own copy once the cache is live
    std::string weight_cache_file_path;
    if (!erlang::nif::check_nil(env, argv[2])) {
        if (!erlang::nif::get(env, argv[2], weight_cache_file_path)) {
            return erlang::nif::error(env, "expecting weight_cache_file_path to be a string or nil");
        }

        res->owned_path = (char *)enif_alloc(weight_cache_file_path.size() + 1);
        if (res->owned_path == nullptr) {
            return erlang::nif::error(env, "cannot allocate memory for weight_cache_file_path");
        }
        memcpy(res->owned_path, weight_cache_file_path.c_str(), weight_cache_file_path.size() + 1);
        options.weight_cache_file_path = res->owned_path;
    }

    res->val = TfLiteXNNPackDelegateCreate(&options);
    if (res->val == nullptr) {
        return erlang::nif::error(env, "cannot create XNNPACK delegate");
    }
    res->deleter = TfLiteXNNPackDelegateDelete;

    ret = enif_make_resource(env, res);
    return erlang::nif::ok(env, ret);
}

#endif

// The plugin's own account of why it refused, which TfLite's wrapper asks for
// and then throws away by passing nullptr. Written and read on the one thread
// inside a single create call.
static thread_local std::string external_delegate_error;

static void capture_external_delegate_error(const char * message) {
    if (message == nullptr) return;
    if (!external_delegate_error.empty()) external_delegate_error += "; ";
    external_delegate_error += message;
}

// Deliberately not TfLiteExternalDelegateCreate. That returns a pointer into an
// ExternalDelegateWrapper whose TfLiteDelegate member it only fills in when the
// library loaded *and* the plugin returned a delegate (external_delegate.cc:147-177);
// on either failure the caller gets a non-null delegate whose Prepare is
// indeterminate, and the guard on the way out tests a plain `new` for null, so it
// never fires. Handing that to ModifyGraphWithDelegate jumps through a wild
// function pointer and takes the VM with it. Loading the plugin here instead
// costs about thirty lines, has no such gap, and gives every failure a name.
ERL_NIF_TERM delegate_external_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);

    std::string library_path;
    if (!erlang::nif::get(env, argv[0], library_path)) {
        return erlang::nif::error(env, "expecting library_path to be a string");
    }

    std::vector<std::string> keys, values;
    if (!get_string_pairs(env, argv[1], keys, values)) {
        return erlang::nif::error(env, "expecting options to be a list of {key, value} strings");
    }

    void * handle = tflite::SharedLibrary::LoadLibrary(library_path.c_str());
    if (handle == nullptr) {
        const char * why = tflite::SharedLibrary::GetError();
        return erlang::nif::error(env, ("cannot load delegate library: " + std::string(why ? why : "unknown error")).c_str());
    }

    // never unloaded, matching TfLite: SharedLibrary::UnLoadLibrary has no call
    // site anywhere in tensorflow/lite, and an interpreter outliving a closed
    // plugin would be far worse than a leaked loader reference.
    //
    // The reference the loader keeps is not a leak worth chasing: measured on
    // macOS, twenty thousand repeat dlopens of one path return the same handle
    // and stop costing anything after 48kB. What does accumulate is one mapping
    // per distinct plugin path, which is the price of never closing one.
    auto create = reinterpret_cast<decltype(&tflite_plugin_create_delegate)>(
        tflite::SharedLibrary::GetLibrarySymbol(handle, "tflite_plugin_create_delegate"));
    auto destroy = reinterpret_cast<decltype(&tflite_plugin_destroy_delegate)>(
        tflite::SharedLibrary::GetLibrarySymbol(handle, "tflite_plugin_destroy_delegate"));
    if (create == nullptr || destroy == nullptr) {
        // Keeping the handle is deliberate once a delegate exists, for the reason
        // above. Here none does and none can, so the loader reference is only a
        // reference to a library nothing will ever call: close it.
        tflite::SharedLibrary::UnLoadLibrary(handle);
        return erlang::nif::error(env, "library is not a TfLite delegate plugin: it exports no tflite_plugin_create_delegate/tflite_plugin_destroy_delegate");
    }

    std::vector<const char *> key_ptrs, value_ptrs;
    for (size_t i = 0; i < keys.size(); i++) {
        key_ptrs.push_back(keys[i].c_str());
        value_ptrs.push_back(values[i].c_str());
    }

    external_delegate_error.clear();
    TfLiteDelegate * delegate = create(
        key_ptrs.empty() ? nullptr : key_ptrs.data(),
        value_ptrs.empty() ? nullptr : value_ptrs.data(),
        key_ptrs.size(),
        capture_external_delegate_error);

    if (delegate == nullptr) {
        std::string reason = "the delegate plugin declined to create a delegate";
        if (!external_delegate_error.empty()) {
            reason += ": " + external_delegate_error;
        }
        // same as above: the plugin refused, so nothing from it is in use
        tflite::SharedLibrary::UnLoadLibrary(handle);
        return erlang::nif::error(env, reason.c_str());
    }

    NifResDelegate * res = nullptr;
    ERL_NIF_TERM ret;
    if (!(res = NifResDelegate::allocate_resource(env, ret))) {
        destroy(delegate);
        tflite::SharedLibrary::UnLoadLibrary(handle);
        return ret;
    }
    ResourceRef<NifResDelegate> hold(res);

    res->val = delegate;
    res->deleter = destroy;

    ret = enif_make_resource(env, res);
    return erlang::nif::ok(env, ret);
}
