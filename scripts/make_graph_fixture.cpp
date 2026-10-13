#include <cstdio>
#include <cstring>
#include <memory>
#include <vector>

#include "flatbuffers/flatbuffers.h"
#include "flatbuffers/flexbuffers.h"
#include "tensorflow/lite/schema/schema_generated.h"

static uint32_t buffer(tflite::ModelT &model, const std::vector<int32_t> &values) {
    auto result = std::make_unique<tflite::BufferT>();
    result->data.resize(values.size() * sizeof(int32_t));
    memcpy(result->data.data(), values.data(), result->data.size());
    model.buffers.push_back(std::move(result));
    return model.buffers.size() - 1;
}

static void tensor(tflite::SubGraphT &graph, tflite::TensorType type,
                   std::vector<int32_t> shape, uint32_t data, const char *name) {
    auto result = std::make_unique<tflite::TensorT>();
    result->type = type;
    result->shape = std::move(shape);
    result->buffer = data;
    result->name = name;
    graph.tensors.push_back(std::move(result));
}

static void signature(tflite::ModelT &model, uint32_t index) {
    auto result = std::make_unique<tflite::SignatureDefT>();
    auto &graph = *model.subgraphs[index];
    result->signature_key = graph.name;
    result->subgraph_index = index;
    auto append = [&graph](const std::vector<int32_t> &indices,
                          std::vector<std::unique_ptr<tflite::TensorMapT>> &maps) {
        for (int32_t i : indices) {
            auto entry = std::make_unique<tflite::TensorMapT>();
            entry->name = graph.tensors[i]->name;
            entry->tensor_index = i;
            maps.push_back(std::move(entry));
        }
    };
    append(graph.inputs, result->inputs);
    append(graph.outputs, result->outputs);
    model.signature_defs.push_back(std::move(result));
}

int main(int argc, char **argv) {
    if (argc != 2 && argc != 3) return 2;
    tflite::ModelT model;
    model.version = 3;
    model.description = "LiteRT graph option fixtures";
    model.buffers.push_back(std::make_unique<tflite::BufferT>());
    for (auto code : {tflite::BuiltinOperator_ONE_HOT, tflite::BuiltinOperator_SPLIT_V,
                     tflite::BuiltinOperator_STABLEHLO_COMPOSITE}) {
        auto entry = std::make_unique<tflite::OperatorCodeT>();
        entry->builtin_code = code;
        entry->deprecated_builtin_code = code < 127 ? code : 127;
        model.operator_codes.push_back(std::move(entry));
    }

    auto one_hot = std::make_unique<tflite::SubGraphT>();
    one_hot->name = "one_hot";
    tensor(*one_hot, tflite::TensorType_INT32, {2}, 0, "indices");
    tensor(*one_hot, tflite::TensorType_INT32, {}, buffer(model, {3}), "depth");
    tensor(*one_hot, tflite::TensorType_FLOAT32, {}, buffer(model, {0x3f800000}), "on");
    tensor(*one_hot, tflite::TensorType_FLOAT32, {}, buffer(model, {0}), "off");
    tensor(*one_hot, tflite::TensorType_FLOAT32, {2, 3}, 0, "one_hot_output");
    auto op = std::make_unique<tflite::OperatorT>();
    op->opcode_index = 0;
    op->inputs = {0, 1, 2, 3};
    op->outputs = {4};
    tflite::OneHotOptionsT hot_options;
    hot_options.axis = -1;
    op->builtin_options.Set(hot_options);
    one_hot->operators.push_back(std::move(op));
    one_hot->inputs = {0};
    one_hot->outputs = {4};
    model.subgraphs.push_back(std::move(one_hot));

    auto split = std::make_unique<tflite::SubGraphT>();
    split->name = "split_v";
    tensor(*split, tflite::TensorType_FLOAT32, {6}, 0, "split_input");
    tensor(*split, tflite::TensorType_INT32, {2}, buffer(model, {2, 4}), "sizes");
    tensor(*split, tflite::TensorType_INT32, {}, buffer(model, {0}), "axis");
    tensor(*split, tflite::TensorType_FLOAT32, {2}, 0, "first");
    tensor(*split, tflite::TensorType_FLOAT32, {4}, 0, "second");
    op = std::make_unique<tflite::OperatorT>();
    op->opcode_index = 1;
    op->inputs = {0, 1, 2};
    op->outputs = {3, 4};
    tflite::SplitVOptionsT split_options;
    split_options.num_splits = 2;
    op->builtin_options.Set(split_options);
    split->operators.push_back(std::move(op));
    split->inputs = {0};
    split->outputs = {3, 4};
    model.subgraphs.push_back(std::move(split));

    auto composite = std::make_unique<tflite::SubGraphT>();
    composite->name = "composite";
    tensor(*composite, tflite::TensorType_INT32, {2}, 0, "composite_input");
    tensor(*composite, tflite::TensorType_FLOAT32, {2, 3}, 0, "composite_output");
    op = std::make_unique<tflite::OperatorT>();
    op->opcode_index = 2;
    op->inputs = {0};
    op->outputs = {1};
    tflite::StableHLOCompositeOptionsT composite_options;
    composite_options.name = "test.one_hot";
    composite_options.decomposition_subgraph_index = 0;
    composite_options.version = 1;
    flexbuffers::Builder attributes;
    auto map = attributes.StartMap();
    attributes.EndMap(map);
    attributes.Finish();
    composite_options.composite_attributes = attributes.GetBuffer();
    op->builtin_options_2.Set(composite_options);
    composite->operators.push_back(std::move(op));
    composite->inputs = {0};
    composite->outputs = {1};
    model.subgraphs.push_back(std::move(composite));
    for (uint32_t i = 0; i < model.subgraphs.size(); ++i) signature(model, i);
    if (argc == 3) {
        if (std::strcmp(argv[2], "mismatched") == 0) {
            model.subgraphs[0]->operators[0]->builtin_options.Set(split_options);
        } else if (std::strcmp(argv[2], "split_mismatched") == 0) {
            model.subgraphs[1]->operators[0]->builtin_options.Set(hot_options);
        } else if (std::strcmp(argv[2], "composite_mismatched") == 0) {
            tflite::StablehloConcatenateOptionsT other;
            model.subgraphs[2]->operators[0]->builtin_options_2.Set(other);
        } else if (std::strcmp(argv[2], "missing") == 0) {
            model.subgraphs[0]->operators[0]->builtin_options.Reset();
        } else {
            return 2;
        }
    }

    flatbuffers::FlatBufferBuilder builder;
    tflite::FinishModelBuffer(builder, tflite::Model::Pack(builder, &model));
    FILE *out = fopen(argv[1], "wb");
    if (!out) return 1;
    size_t written = fwrite(builder.GetBufferPointer(), 1, builder.GetSize(), out);
    int status = fclose(out);
    return written == builder.GetSize() && status == 0 ? 0 : 1;
}
