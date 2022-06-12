#ifndef TFLITE_CORAL_BINDINGS_H
#define TFLITE_CORAL_BINDINGS_H

#include "../nif_utils.hpp"
#include "coral/tflite_utils.h"
#include "tflite/public/edgetpu.h"
#include "tflite/public/edgetpu_c.h"
#include <vector>
#include <string.h>

#pragma once
#define EDGETPU_DEVICE_NAME_BUFFER_SIZE 64

std::map<void *, std::shared_ptr<edgetpu::EdgeTpuContext>> managedContext;

using NifResEdgeTpuContext = erlang_nif_res<edgetpu::EdgeTpuContext *>;

static void destruct_egdetpu_context(ErlNifEnv *env, void *args) {
    auto res = (NifResEdgeTpuContext *)args;
    if (res->val) {
        auto iter = managedContext.find(res->val);
        if (iter != managedContext.end()) {
            managedContext.erase(iter);
        }
        res->val = nullptr;
    }
}

static ERL_NIF_TERM coral_contains_edgetpu_custom_op(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

static ERL_NIF_TERM coral_edgetpu_devices(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

static ERL_NIF_TERM coral_get_edgetpu_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
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

#endif // TFLITE_CORAL_BINDINGS_H
