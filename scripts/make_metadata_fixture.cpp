// Builds test/models/metadata_corners.bin, a model whose TFLITE_METADATA block
// reaches the parts of the metadata schema that no model in the test corpus
// touches: a FeatureProperties content, a score thresholding unit, a
// SentencePiece tokenizer with no vocab file, and a custom metadata entry.
//
// One shot; the output is committed. To regenerate, see the compile line in
// scripts/make_metadata_fixture.sh.
#include <cstdio>
#include <vector>

#include "flatbuffers/flatbuffers.h"
#include "metadata_schema_generated.h"
#include "tensorflow/lite/schema/schema_generated.h"

namespace md = tflite;

static flatbuffers::DetachedBuffer build_metadata() {
    flatbuffers::FlatBufferBuilder fbb;

    // Tensor 0: content is FeatureProperties, an empty marker table. Its range
    // is readable and has to survive alongside it.
    auto feature_content = md::CreateContent(
        fbb, md::ContentProperties_FeatureProperties,
        md::CreateFeatureProperties(fbb).Union(),
        md::CreateValueRange(fbb, 1, 1));
    auto t0 = md::CreateTensorMetadataDirect(
        fbb, "features", "a plain feature vector", nullptr, feature_content);

    // Tensor 1: a thresholding unit next to a normalization one, so the key the
    // threshold is reported under is visible and the sibling proves the list
    // itself came through.
    std::vector<float> mean1{127.5f}, std1{127.5f};
    std::vector<flatbuffers::Offset<md::ProcessUnit>> units1;
    units1.push_back(md::CreateProcessUnit(
        fbb, md::ProcessUnitOptions_ScoreThresholdingOptions,
        md::CreateScoreThresholdingOptions(fbb, 0.25f).Union()));
    units1.push_back(md::CreateProcessUnit(
        fbb, md::ProcessUnitOptions_NormalizationOptions,
        md::CreateNormalizationOptionsDirect(fbb, &mean1, &std1).Union()));
    auto t1 = md::CreateTensorMetadataDirect(
        fbb, "scores", "scores with a threshold", nullptr, 0, &units1);

    // Tensor 2: a SentencePiece tokenizer carrying a model file but no vocab
    // file, which the schema permits, again beside a normalization unit.
    std::vector<flatbuffers::Offset<md::AssociatedFile>> sp_model;
    sp_model.push_back(md::CreateAssociatedFileDirect(
        fbb, "sp.model", "the sentencepiece model",
        md::AssociatedFileType_VOCABULARY));
    std::vector<float> mean2{0.0f}, std2{1.0f};
    std::vector<flatbuffers::Offset<md::ProcessUnit>> units2;
    units2.push_back(md::CreateProcessUnit(
        fbb, md::ProcessUnitOptions_SentencePieceTokenizerOptions,
        md::CreateSentencePieceTokenizerOptions(
            fbb, fbb.CreateVector(sp_model), 0)
            .Union()));
    units2.push_back(md::CreateProcessUnit(
        fbb, md::ProcessUnitOptions_NormalizationOptions,
        md::CreateNormalizationOptionsDirect(fbb, &mean2, &std2).Union()));
    auto t2 = md::CreateTensorMetadataDirect(
        fbb, "tokens", "sentencepiece without a vocab file", nullptr, 0,
        &units2);

    std::vector<flatbuffers::Offset<md::TensorMetadata>> inputs{t0, t1, t2};

    std::vector<uint8_t> blob{1, 2, 3, 4};
    std::vector<flatbuffers::Offset<md::CustomMetadata>> custom;
    custom.push_back(md::CreateCustomMetadataDirect(fbb, "beam_test", &blob));

    auto subgraph = md::CreateSubGraphMetadataDirect(
        fbb, "corners", "every corner this repo did not otherwise reach",
        &inputs, nullptr, nullptr, nullptr, nullptr, nullptr, nullptr,
        &custom);
    std::vector<flatbuffers::Offset<md::SubGraphMetadata>> subgraphs{subgraph};

    auto root = md::CreateModelMetadataDirect(
        fbb, "metadata corners", "fixture", "v1", &subgraphs, "tflite_beam",
        "Apache-2.0", nullptr, "1.0.0");
    md::FinishModelMetadataBuffer(fbb, root);
    return fbb.Release();
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s <out.bin>\n", argv[0]);
        return 2;
    }
    auto metadata = build_metadata();

    flatbuffers::FlatBufferBuilder fbb;
    // Buffer 0 is required to be empty, buffer 1 carries the metadata block.
    std::vector<flatbuffers::Offset<tflite::Buffer>> buffers;
    buffers.push_back(tflite::CreateBuffer(fbb, fbb.CreateVector<uint8_t>({})));
    buffers.push_back(tflite::CreateBuffer(
        fbb, fbb.CreateVector(metadata.data(), metadata.size())));

    std::vector<int32_t> shape{1, 4};
    std::vector<flatbuffers::Offset<tflite::Tensor>> tensors;
    const char* names[] = {"features", "scores", "tokens"};
    for (int i = 0; i < 3; i++) {
        tensors.push_back(tflite::CreateTensorDirect(
            fbb, &shape, tflite::TensorType_FLOAT32, 0, names[i]));
    }

    std::vector<int32_t> inputs{0, 1, 2};
    std::vector<int32_t> outputs{0};
    auto subgraph = tflite::CreateSubGraphDirect(
        fbb, &tensors, &inputs, &outputs, nullptr, "main");
    std::vector<flatbuffers::Offset<tflite::SubGraph>> subgraphs{subgraph};

    std::vector<flatbuffers::Offset<tflite::Metadata>> model_metadata;
    model_metadata.push_back(
        tflite::CreateMetadataDirect(fbb, "TFLITE_METADATA", 1));

    auto model = tflite::CreateModelDirect(
        fbb, 3, nullptr, &subgraphs, "metadata corners",
        &buffers, nullptr, &model_metadata);
    tflite::FinishModelBuffer(fbb, model);

    FILE* f = fopen(argv[1], "wb");
    if (!f) { perror("fopen"); return 1; }
    fwrite(fbb.GetBufferPointer(), 1, fbb.GetSize(), f);
    fclose(f);
    printf("wrote %s, %u bytes\n", argv[1], fbb.GetSize());
    return 0;
}
