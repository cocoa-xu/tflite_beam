// Builds test/models/fp8_types.bin, a model whose tensors carry the two 8 bit
// float types. No model in the corpus has one, and the two are impossible to
// tell apart by width alone, which is the whole reason they are reported by
// name.
//
// One shot; the output is committed. See scripts/make_fp8_fixture.sh.
#include <cstdio>
#include <vector>

#include "flatbuffers/flatbuffers.h"
#include "tflite/schema/schema_generated.h"

int main(int argc, char** argv) {
    if (argc != 2) { fprintf(stderr, "usage: %s <out.bin>\n", argv[0]); return 2; }
    flatbuffers::FlatBufferBuilder fbb;

    std::vector<flatbuffers::Offset<tflite::Buffer>> buffers;
    buffers.push_back(tflite::CreateBuffer(fbb, fbb.CreateVector<uint8_t>({})));

    std::vector<int32_t> shape{1, 4};
    std::vector<flatbuffers::Offset<tflite::Tensor>> tensors;
    tensors.push_back(tflite::CreateTensorDirect(
        fbb, &shape, tflite::TensorType_FLOAT8_E4M3FN, 0, "e4m3fn"));
    tensors.push_back(tflite::CreateTensorDirect(
        fbb, &shape, tflite::TensorType_FLOAT8_E5M2, 0, "e5m2"));
    tensors.push_back(tflite::CreateTensorDirect(
        fbb, &shape, tflite::TensorType_FLOAT32, 0, "f32"));

    std::vector<int32_t> inputs{0, 1};
    std::vector<int32_t> outputs{2};
    auto subgraph = tflite::CreateSubGraphDirect(
        fbb, &tensors, &inputs, &outputs, nullptr, "main");
    std::vector<flatbuffers::Offset<tflite::SubGraph>> subgraphs{subgraph};

    auto model = tflite::CreateModelDirect(
        fbb, 3, nullptr, &subgraphs, "fp8 types", &buffers);
    tflite::FinishModelBuffer(fbb, model);

    FILE* f = fopen(argv[1], "wb");
    if (!f) { perror("fopen"); return 1; }
    fwrite(fbb.GetBufferPointer(), 1, fbb.GetSize(), f);
    fclose(f);
    printf("wrote %s, %u bytes\n", argv[1], fbb.GetSize());
    return 0;
}
