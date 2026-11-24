#ifdef TFLITE_BEAM_LITERT_API_ENABLED
#include <erl_nif.h>

#include <cstring>
#include <memory>
#include <limits>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#include "../erlang_nif_resource.h"
#include "../nif_utils.hpp"
#include "flatbuffers/flexbuffers.h"
#include "litert/c/litert_model.h"
#include "litert/c/litert_op_options.h"
#include "litert/core/model/model.h"
#include "litert/core/model/model_serialize.h"

namespace {

void check(LiteRtStatus status) {
    if (status != kLiteRtStatusOk) throw std::runtime_error(LiteRtGetStatusString(status));
}

ERL_NIF_TERM atom(ErlNifEnv *env, const char *name) {
    return erlang::nif::atom(env, name);
}

ERL_NIF_TERM binary(ErlNifEnv *env, const void *data, size_t size) {
    ERL_NIF_TERM term;
    unsigned char *out = enif_make_new_binary(env, size, &term);
    if (!out && size) throw std::bad_alloc();
    if (size) memcpy(out, data, size);
    return term;
}

void put(ErlNifEnv *env, ERL_NIF_TERM &map, const char *key, ERL_NIF_TERM value) {
    if (!enif_make_map_put(env, map, atom(env, key), value, &map)) throw std::bad_alloc();
}

ERL_NIF_TERM get(ErlNifEnv *env, ERL_NIF_TERM map, const char *key) {
    ERL_NIF_TERM value;
    if (!enif_get_map_value(env, map, atom(env, key), &value)) {
        throw std::invalid_argument(std::string("missing graph option: ") + key);
    }
    return value;
}

int integer(ErlNifEnv *env, ERL_NIF_TERM map, const char *key) {
    int value;
    if (!enif_get_int(env, get(env, map, key), &value)) {
        throw std::invalid_argument(std::string("expecting a signed 32-bit integer: ") + key);
    }
    return value;
}

std::string string(ErlNifEnv *env, ERL_NIF_TERM term) {
    ErlNifBinary value;
    if (!enif_inspect_binary(env, term, &value)) throw std::invalid_argument("expecting a binary string");
    std::string result;
    if (value.size) result.assign(reinterpret_cast<const char *>(value.data), value.size);
    if (result.find('\0') != std::string::npos) throw std::invalid_argument("string contains NUL");
    return result;
}

using Model = std::unique_ptr<LiteRtModelT, decltype(&LiteRtDestroyModel)>;

void validate_options(LiteRtOp op) {
    bool valid = true;
    switch (op->OpCode()) {
    case kLiteRtOpCodeTflOneHot:
        valid = litert::internal::GetTflOptions(*op).AsOneHotOptions() != nullptr;
        break;
    case kLiteRtOpCodeTflSplitV:
        valid = litert::internal::GetTflOptions(*op).AsSplitVOptions() != nullptr;
        break;
    case kLiteRtOpCodeTflConv3dTranspose:
        valid = litert::internal::GetTflOptions(*op).AsConv3DOptions() != nullptr;
        break;
    case kLiteRtOpCodeTflLocalResponseNormalization:
        valid = litert::internal::GetTflOptions(*op).AsLocalResponseNormalizationOptions() != nullptr;
        break;
    case kLiteRtOpCodeShloComposite:
        valid = litert::internal::GetTflOptions2(*op).AsStableHLOCompositeOptions() != nullptr;
        break;
    default: break;
    }
    if (!valid) throw std::invalid_argument("operator options do not match the opcode");
}

ERL_NIF_TERM padding_term(ErlNifEnv *env, uint32_t padding) {
    switch (padding) {
    case 0: return atom(env, "same");
    case 1: return atom(env, "valid");
    default: return enif_make_uint(env, padding);
    }
}

uint32_t padding_value(ErlNifEnv *env, ERL_NIF_TERM term) {
    if (enif_is_identical(term, atom(env, "same"))) return 0;
    if (enif_is_identical(term, atom(env, "valid"))) return 1;
    throw std::invalid_argument("padding must be same or valid");
}

ERL_NIF_TERM activation_term(ErlNifEnv *env, uint32_t activation) {
    switch (activation) {
    case 0: return atom(env, "none");
    case 1: return atom(env, "relu");
    case 2: return atom(env, "relu_n1_to_1");
    case 3: return atom(env, "relu6");
    case 4: return atom(env, "tanh");
    case 5: return atom(env, "sign_bit");
    default: return enif_make_uint(env, activation);
    }
}

uint32_t activation_value(ErlNifEnv *env, ERL_NIF_TERM term) {
    const char *names[] = {"none", "relu", "relu_n1_to_1", "relu6", "tanh", "sign_bit"};
    for (uint32_t i = 0; i < 6; ++i) {
        if (enif_is_identical(term, atom(env, names[i]))) return i;
    }
    throw std::invalid_argument("unknown fused_activation_function");
}

Model load(ErlNifEnv *env, ERL_NIF_TERM environment, ERL_NIF_TERM source) {
    ERL_NIF_TERM error;
    auto resource = NifResLiteRtEnvironment::get_resource(env, environment, error);
    if (!resource) throw std::invalid_argument("expecting a LiteRT environment");
    LiteRtModel model = nullptr;
    int arity;
    const ERL_NIF_TERM *tuple;
    if (enif_get_tuple(env, source, &arity, &tuple) && arity == 2 &&
        enif_is_identical(tuple[0], atom(env, "buffer"))) {
        ErlNifBinary bytes;
        if (!enif_inspect_binary(env, tuple[1], &bytes)) {
            throw std::invalid_argument("expecting {buffer, binary model}");
        }
        check(LiteRtCreateModelFromBuffer(resource->val, bytes.data, bytes.size, &model));
    } else {
        std::string path = string(env, source);
        check(LiteRtCreateModelFromFile(resource->val, path.c_str(), &model));
    }
    Model result(model, LiteRtDestroyModel);
    if (model->NumSubgraphs() == 0) throw std::invalid_argument("model has no subgraphs");
    for (size_t i = 0; i < model->NumSubgraphs(); ++i) {
        for (LiteRtOp op : model->Subgraph(i).Ops()) validate_options(op);
    }
    return result;
}

ERL_NIF_TERM tensor_indices(ErlNifEnv *env, const std::vector<LiteRtTensor> &tensors) {
    ERL_NIF_TERM list = enif_make_list(env, 0);
    for (size_t i = tensors.size(); i-- > 0;) {
        uint32_t index;
        check(LiteRtGetTensorIndex(tensors[i], &index));
        list = enif_make_list_cell(env, enif_make_uint(env, index), list);
    }
    return list;
}

ERL_NIF_TERM options(ErlNifEnv *env, LiteRtOp op) {
    ERL_NIF_TERM result = enif_make_new_map(env);
    int32_t value;
    switch (op->OpCode()) {
    case kLiteRtOpCodeTflOneHot:
        check(LiteRtGetOneHotAxisOption(op, &value));
        put(env, result, "axis", enif_make_int(env, value));
        break;
    case kLiteRtOpCodeTflSplitV:
        check(LiteRtGetSplitVNumSplitsOption(op, &value));
        put(env, result, "num_splits", enif_make_int(env, value));
        break;
    case kLiteRtOpCodeTflConv3dTranspose: {
        uint32_t padding = 0, activation = 0;
        int32_t stride_d = 0, stride_w = 0, stride_h = 0;
        int32_t dilation_d = 0, dilation_w = 0, dilation_h = 0;
        check(LiteRtGetConv3dTransposePaddingOption(op, &padding));
        check(LiteRtGetConv3dTransposeStrideDOption(op, &stride_d));
        check(LiteRtGetConv3dTransposeStrideWOption(op, &stride_w));
        check(LiteRtGetConv3dTransposeStrideHOption(op, &stride_h));
        check(LiteRtGetConv3dTransposeFusedActivationOption(op, &activation));
        check(LiteRtGetConv3dTransposeDilationDOption(op, &dilation_d));
        check(LiteRtGetConv3dTransposeDilationWOption(op, &dilation_w));
        check(LiteRtGetConv3dTransposeDilationHOption(op, &dilation_h));
        put(env, result, "padding", padding_term(env, padding));
        put(env, result, "stride_d", enif_make_int(env, stride_d));
        put(env, result, "stride_w", enif_make_int(env, stride_w));
        put(env, result, "stride_h", enif_make_int(env, stride_h));
        put(env, result, "fused_activation_function", activation_term(env, activation));
        put(env, result, "dilation_d_factor", enif_make_int(env, dilation_d));
        put(env, result, "dilation_w_factor", enif_make_int(env, dilation_w));
        put(env, result, "dilation_h_factor", enif_make_int(env, dilation_h));
        break;
    }
    case kLiteRtOpCodeTflLocalResponseNormalization: {
        int32_t radius = 0;
        float bias = 0.0f, alpha = 0.0f, beta = 0.0f;
        check(LiteRtGetLocalResponseNormalizationRadiusOption(op, &radius));
        check(LiteRtGetLocalResponseNormalizationBiasOption(op, &bias));
        check(LiteRtGetLocalResponseNormalizationAlphaOption(op, &alpha));
        check(LiteRtGetLocalResponseNormalizationBetaOption(op, &beta));
        put(env, result, "radius", enif_make_int(env, radius));
        put(env, result, "bias", enif_make_double(env, bias));
        put(env, result, "alpha", enif_make_double(env, alpha));
        put(env, result, "beta", enif_make_double(env, beta));
        break;
    }
    case kLiteRtOpCodeShloComposite: {
        const char *name;
        const uint8_t *attributes;
        int32_t size;
        check(LiteRtGetSHLOCompositeOpName(op, &name));
        put(env, result, "name", binary(env, name, strlen(name)));
        check(LiteRtGetSHLOCompositeOpDecompositionSubgraphIndex(op, &value));
        put(env, result, "decomposition_subgraph_index", enif_make_int(env, value));
        check(LiteRtGetSHLOCompositeOpVersion(op, &value));
        put(env, result, "version", enif_make_int(env, value));
        check(LiteRtGetSHLOCompositeOpAttributes(op, &attributes, &size));
        if (size < 0) throw std::invalid_argument("composite attributes are too large");
        put(env, result, "attributes", binary(env, attributes, size));
        break;
    }
    default: break;
    }
    return result;
}

ERL_NIF_TERM code(ErlNifEnv *env, LiteRtOpCode value) {
    switch (value) {
    case kLiteRtOpCodeTflOneHot: return atom(env, "one_hot");
    case kLiteRtOpCodeTflSplitV: return atom(env, "split_v");
    case kLiteRtOpCodeTflConv3dTranspose: return atom(env, "conv3d_transpose");
    case kLiteRtOpCodeTflLocalResponseNormalization: return atom(env, "local_response_normalization");
    case kLiteRtOpCodeShloComposite: return atom(env, "stablehlo_composite");
    default: return enif_make_int(env, value);
    }
}

void check_decomposition(LiteRtModel model, size_t source, int target, LiteRtOp op) {
    if (target < 0 || static_cast<size_t>(target) >= model->NumSubgraphs()) {
        throw std::invalid_argument("decomposition subgraph index is out of range");
    }
    auto &graph = model->Subgraph(target);
    if (graph.Inputs().size() != op->Inputs().size() || graph.Outputs().size() != op->Outputs().size()) {
        throw std::invalid_argument("decomposition inputs and outputs must match the composite");
    }
    std::vector<unsigned char> state(model->NumSubgraphs());
    std::vector<std::pair<size_t, bool>> pending{{static_cast<size_t>(target), false}};
    while (!pending.empty()) {
        auto [current, leaving] = pending.back();
        pending.pop_back();
        if (leaving) { state[current] = 2; continue; }
        if (current == source) throw std::invalid_argument("recursive composite decomposition");
        if (state[current] == 1) throw std::invalid_argument("recursive nested composite decomposition");
        if (state[current] == 2) continue;
        state[current] = 1;
        pending.emplace_back(current, true);
        for (LiteRtOp child : model->Subgraph(current).Ops()) {
            if (child->OpCode() != kLiteRtOpCodeShloComposite) continue;
            int32_t next;
            check(LiteRtGetSHLOCompositeOpDecompositionSubgraphIndex(child, &next));
            if (next < 0 || static_cast<size_t>(next) >= model->NumSubgraphs()) {
                throw std::invalid_argument("invalid nested composite decomposition");
            }
            pending.emplace_back(next, false);
        }
    }
}

void rewrite_options(ErlNifEnv *env, LiteRtModel model, size_t graph_index,
                     LiteRtOp original, ERL_NIF_TERM map) {
    size_t size;
    if (!enif_get_map_size(env, map, &size)) throw std::invalid_argument("expecting an operator options map");
    LiteRtBuilderT builder;
    LiteRtOp op = &builder.BuildOp(original->OpCode(), {}, {});
    switch (original->OpCode()) {
    case kLiteRtOpCodeTflOneHot: {
        if (size != 1) throw std::invalid_argument("one_hot accepts only axis");
        int32_t axis = integer(env, map, "axis");
        if (original->Inputs().size() != 4) throw std::invalid_argument("one_hot needs four inputs");
        LiteRtRankedTensorType input;
        check(LiteRtGetRankedTensorType(original->Inputs()[0], &input));
        if (axis < -1 || axis > static_cast<int32_t>(input.layout.rank)) {
            throw std::invalid_argument("one_hot axis is outside the output rank");
        }
        check(LiteRtBuilderBuildOneHotOpOption(&builder, op, &axis));
        litert::internal::SetTflOptions(*original, litert::internal::TakeTflOptions(*op));
        break;
    }
    case kLiteRtOpCodeTflSplitV: {
        if (size != 1) throw std::invalid_argument("split_v accepts only num_splits");
        int32_t count = integer(env, map, "num_splits");
        if (count <= 0 || static_cast<size_t>(count) != original->Outputs().size()) {
            throw std::invalid_argument("num_splits must match the existing output count");
        }
        check(LiteRtBuilderBuildSplitVOpOption(&builder, op, &count));
        litert::internal::SetTflOptions(*original, litert::internal::TakeTflOptions(*op));
        break;
    }
    case kLiteRtOpCodeTflConv3dTranspose: {
        if (size != 8) {
            throw std::invalid_argument(
                "conv3d_transpose needs padding, stride_w, stride_h, stride_d, "
                "dilation_w_factor, dilation_h_factor, dilation_d_factor and fused_activation_function");
        }
        uint32_t padding = padding_value(env, get(env, map, "padding"));
        int32_t stride_w = integer(env, map, "stride_w");
        int32_t stride_h = integer(env, map, "stride_h");
        int32_t stride_d = integer(env, map, "stride_d");
        int32_t dilation_w = integer(env, map, "dilation_w_factor");
        int32_t dilation_h = integer(env, map, "dilation_h_factor");
        int32_t dilation_d = integer(env, map, "dilation_d_factor");
        uint32_t activation = activation_value(env, get(env, map, "fused_activation_function"));
        if (stride_w <= 0 || stride_h <= 0 || stride_d <= 0 ||
            dilation_w <= 0 || dilation_h <= 0 || dilation_d <= 0) {
            throw std::invalid_argument("conv3d_transpose strides and dilations must be positive");
        }
        check(LiteRtBuilderBuildConv3dTransposeOpOption(
            &builder, op, &padding, &stride_w, &stride_h, &stride_d,
            &dilation_w, &dilation_h, &dilation_d, &activation));
        litert::internal::SetTflOptions(*original, litert::internal::TakeTflOptions(*op));
        break;
    }
    case kLiteRtOpCodeShloComposite: {
        if (size != 4) throw std::invalid_argument("stablehlo_composite needs name, decomposition_subgraph_index, version and attributes");
        std::string name = string(env, get(env, map, "name"));
        int32_t index = integer(env, map, "decomposition_subgraph_index");
        int32_t version = integer(env, map, "version");
        ErlNifBinary attributes;
        if (version < 0 ||
            !enif_inspect_binary(env, get(env, map, "attributes"), &attributes)) {
            throw std::invalid_argument("invalid stablehlo_composite options");
        }
        if (attributes.size > static_cast<size_t>(std::numeric_limits<int32_t>::max()) ||
            !flexbuffers::VerifyBuffer(attributes.data, attributes.size) ||
            !flexbuffers::GetRoot(attributes.data, attributes.size).IsMap()) {
            throw std::invalid_argument("composite attributes must be a valid FlexBuffers map");
        }
        check_decomposition(model, graph_index, index, original);
        check(LiteRtBuilderBuildShloCompositeOpOption(&builder, op, name.c_str(), &index,
                                                     &version, attributes.data, attributes.size));
        litert::internal::SetTflOptions2(*original, litert::internal::TakeTflOptions2(*op));
        break;
    }
    default: throw std::invalid_argument("graph rewrite supports one_hot, split_v, conv3d_transpose and stablehlo_composite");
    }
}

}  // namespace

ERL_NIF_TERM litert_model_inspect(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 2) return enif_make_badarg(env);
    Model model = load(env, argv[0], argv[1]);
    ERL_NIF_TERM graphs = enif_make_list(env, 0);
    for (size_t i = model->NumSubgraphs(); i-- > 0;) {
        auto &graph = model->Subgraph(i);
        ERL_NIF_TERM operators = enif_make_list(env, 0);
        for (size_t j = graph.Ops().size(); j-- > 0;) {
            LiteRtOp op = graph.Ops()[j];
            ERL_NIF_TERM entry = enif_make_new_map(env);
            put(env, entry, "index", enif_make_uint64(env, j));
            put(env, entry, "code", code(env, op->OpCode()));
            put(env, entry, "inputs", tensor_indices(env, op->Inputs()));
            put(env, entry, "outputs", tensor_indices(env, op->Outputs()));
            put(env, entry, "options", options(env, op));
            operators = enif_make_list_cell(env, entry, operators);
        }
        ERL_NIF_TERM entry = enif_make_new_map(env);
        put(env, entry, "index", enif_make_uint64(env, i));
        put(env, entry, "inputs", tensor_indices(env, graph.Inputs()));
        put(env, entry, "outputs", tensor_indices(env, graph.Outputs()));
        put(env, entry, "operators", operators);
        graphs = enif_make_list_cell(env, entry, graphs);
    }
    return erlang::nif::ok(env, graphs);
}

ERL_NIF_TERM litert_model_rewrite(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    if (argc != 3) return enif_make_badarg(env);
    unsigned count;
    if (!enif_get_list_length(env, argv[2], &count)) {
        throw std::invalid_argument("expecting a list of graph edits");
    }
    Model model = load(env, argv[0], argv[1]);
    ERL_NIF_TERM head, tail = argv[2];
    while (enif_get_list_cell(env, tail, &head, &tail)) {
        size_t size;
        if (!enif_get_map_size(env, head, &size) || size != 3) {
            throw std::invalid_argument("each graph edit needs subgraph, operator and options");
        }
        int graph_index = integer(env, head, "subgraph");
        int op_index = integer(env, head, "operator");
        if (graph_index < 0 || static_cast<size_t>(graph_index) >= model->NumSubgraphs()) {
            throw std::invalid_argument("subgraph index is out of range");
        }
        auto &graph = model->Subgraph(graph_index);
        if (op_index < 0 || static_cast<size_t>(op_index) >= graph.Ops().size()) {
            throw std::invalid_argument("operator index is out of range");
        }
        rewrite_options(env, model.get(), graph_index, graph.Ops()[op_index], get(env, head, "options"));
    }
    auto serialized = litert::internal::SerializeModel(std::move(*model), 64);
    if (!serialized) throw std::runtime_error(serialized.Error().Message());
    return erlang::nif::ok(env, binary(env, serialized->Data(), serialized->Size()));
}
#endif
