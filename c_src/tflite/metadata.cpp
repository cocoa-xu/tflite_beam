#include <map>
#include <string>
#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "flatbuffers/flatbuffers.h"
#include "../metadata_schema_generated.h"

#include "metadata.h"

ERL_NIF_TERM tflite_metadata_associated_files_to_erl_term(ErlNifEnv *env, const flatbuffers::Vector<flatbuffers::Offset<tflite::AssociatedFile>> * associated_files);

ERL_NIF_TERM tflite_metadata_to_erl_term(ErlNifEnv *env, const void *buf) {
    if (!buf) {
        return erlang::nif::atom(env, "nil");
    }

    const tflite::ModelMetadata * metadata = tflite::GetModelMetadata(buf);
    if (!metadata) {
        return erlang::nif::atom(env, "nil");
    }

    std::map<std::string, std::string> metadata_;
    metadata_["name"] = metadata->name()->str();
    metadata_["description"] = metadata->description()->str();
    metadata_["version"] = metadata->version()->str();

    const flatbuffers::Vector<flatbuffers::Offset<tflite::AssociatedFile>> * associated_files = metadata->associated_files();
    ERL_NIF_TERM associated_files_term = tflite_metadata_associated_files_to_erl_term(env, associated_files);

    ERL_NIF_TERM metadata_term;
    if (erlang::nif::make(env, metadata_, metadata_term)) {
        return erlang::nif::error(env, "enif_alloc failed");
    }

    ERL_NIF_TERM metadata_term_updated;
    enif_make_map_put(env, metadata_term, erlang::nif::make_binary(env, "associated_files"), associated_files_term, &metadata_term_updated);

    return metadata_term_updated;
}

ERL_NIF_TERM tflite_metadata_associated_files_to_erl_term(ErlNifEnv *env, const flatbuffers::Vector<flatbuffers::Offset<tflite::AssociatedFile>> * associated_files) {
    std::vector<std::map<std::string, std::string>> associated_files_;
    if (associated_files) {
        for (size_t i = 0; i < associated_files->size(); i++) {
            std::map<std::string, std::string> associated_file_;
            auto associated_file = associated_files->Get(i);
            associated_file_["name"] = associated_file->name()->str();
            associated_file_["description"] = associated_file->description()->str();
            associated_file_["locale"] = associated_file->locale()->str();
            associated_file_["version"] = associated_file->version()->str();

            switch (associated_file->type()) {
                case tflite::AssociatedFileType::AssociatedFileType_DESCRIPTIONS:
                    associated_file_["type"] = "descriptions";
                    break;
                case tflite::AssociatedFileType::AssociatedFileType_TENSOR_AXIS_LABELS:
                    associated_file_["type"] = "tensor_axis_labels";
                    break;
                case tflite::AssociatedFileType::AssociatedFileType_TENSOR_VALUE_LABELS:
                    associated_file_["type"] = "tensor_value_labels";
                    break;
                case tflite::AssociatedFileType::AssociatedFileType_TENSOR_AXIS_SCORE_CALIBRATION:
                    associated_file_["type"] = "tensor_axis_score_calibration";
                    break;
                case tflite::AssociatedFileType::AssociatedFileType_VOCABULARY:
                    associated_file_["type"] = "vocabulary";
                    break;
                case tflite::AssociatedFileType::AssociatedFileType_SCANN_INDEX_FILE:
                    associated_file_["type"] = "scann_index_file";
                    break;
                default:
                    associated_file_["type"] = "unknown";
                    break;
            }
            associated_files_.emplace_back(associated_file_);
        }
    }

    ERL_NIF_TERM associated_file_term;
    if (erlang::nif::make(env, associated_files_, associated_file_term)) {
        return erlang::nif::error(env, "enif_alloc failed");
    }

    return associated_file_term;
}
