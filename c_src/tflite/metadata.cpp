#include <map>
#include <string>
#include <erl_nif.h>
#include "../nif_utils.hpp"
#include "flatbuffers/flatbuffers.h"
#include "../metadata_schema_generated.h"

#include "metadata.h"

bool tflite_metadata_associated_files_to_erl_term(ErlNifEnv *env, const flatbuffers::Vector<flatbuffers::Offset<tflite::AssociatedFile>> * associated_files, ERL_NIF_TERM &out);
bool tflite_metadata_input_tensor_metadata_to_erl_term(ErlNifEnv *env, const flatbuffers::Vector<flatbuffers::Offset<tflite::TensorMetadata>> * subgraph_metadata, ERL_NIF_TERM &out);
void _tflite_metadata_set_key_value(std::map<std::string, std::string>& map, const std::string& key, const flatbuffers::String * value);

template <typename T>
bool _fb_var_to_erl(ErlNifEnv *env, const T* fb_var, ERL_NIF_TERM &out);

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const flatbuffers::String* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    out = erlang::nif::make_binary(env, fb_var->str());
    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::ImageSize* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    ERL_NIF_TERM keys[2], values[2];
    keys[0] = erlang::nif::atom(env, "width");
    keys[1] = erlang::nif::atom(env, "height");
    values[0] = erlang::nif::make(env, fb_var->width());
    values[1] = erlang::nif::make(env, fb_var->height());

    if (!enif_make_map_from_arrays(env, keys, values, 2, &out)) {
        out = erlang::nif::error(env, "duplicate keys");
        return false;
    }

    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::Stats* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    ERL_NIF_TERM keys[2], values[2];

    size_t items = 0;
    auto max_v = fb_var->max();
    if (max_v) {
        ERL_NIF_TERM max_term;
        if (erlang::nif::make_f64_list_from_c_array(env, max_v->size(), max_v->data(), max_term)) {
            out = max_term;
            return false;
        }
        keys[items] = erlang::nif::atom(env, "max");
        values[items] = max_term;
        items++;
    }

    auto min_v = fb_var->min();
    if (min_v) {
        ERL_NIF_TERM min_term;
        if (erlang::nif::make_f64_list_from_c_array(env, min_v->size(), min_v->data(), min_term)) {
            out = min_term;
            return false;
        }
        keys[items] = erlang::nif::atom(env, "min");
        values[items] = min_term;
        items++;
    }

    if (!enif_make_map_from_arrays(env, keys, values, items, &out)) {
        out = erlang::nif::error(env, "duplicate keys");
        return false;
    }

    return true;
}

template <typename V>
bool _fb_var_to_erl(ErlNifEnv *env, const flatbuffers::Vector<flatbuffers::Offset<V>>* fb_vars, ERL_NIF_TERM &out) {
    if (!fb_vars) {
        return false;
    }

    size_t count = fb_vars->size();
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return true;
    }

    ERL_NIF_TERM * erl_terms = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * count);
    if (!erl_terms) {
        out = erlang::nif::error(env, "enif_alloc failed");
        return false;
    }

    size_t valid = 0;
    for (size_t i = 0; i < count; i++) {
        auto fb_var = fb_vars->Get(i);
        if (fb_var) {
            if (!_fb_var_to_erl(env, fb_var, erl_terms[valid])) {
                enif_free(erl_terms);
                out = erl_terms[valid];
                return false;
            }
            valid++;
        }
    }

    out = enif_make_list_from_array(env, erl_terms, valid);
    enif_free(erl_terms);
    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const flatbuffers::Vector<flatbuffers::Offset<tflite::AssociatedFile>>* fb_vars, ERL_NIF_TERM &out) {
    if (!fb_vars) {
        return false;
    }

    size_t count = fb_vars->size();
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return true;
    }

    std::vector<std::map<std::string, std::string>> associated_files_;
    if (fb_vars) {
        for (size_t i = 0; i < fb_vars->size(); i++) {
            std::map<std::string, std::string> associated_file_;
            auto associated_file = fb_vars->Get(i);
            if (associated_file) {
                _tflite_metadata_set_key_value(associated_file_, "name", associated_file->name());
                _tflite_metadata_set_key_value(associated_file_, "description", associated_file->description());
                _tflite_metadata_set_key_value(associated_file_, "locale", associated_file->locale());
                _tflite_metadata_set_key_value(associated_file_, "version", associated_file->version());
            }
            associated_file_["type"] = EnumNameAssociatedFileType(associated_file->type());
            associated_files_.emplace_back(associated_file_);
        }
    }

    if (erlang::nif::make(env, associated_files_, out, true)) {
        out = erlang::nif::error(env, "enif_alloc failed");
        return false;
    }

    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::FeatureProperties* fb_var, ERL_NIF_TERM &out) {
    return false;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::ImageProperties* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    std::map<std::string, std::string> properties;
    properties["color_space"] = EnumNameColorSpaceType(fb_var->color_space());
    if (erlang::nif::make(env, properties, out, true)) {
        out = erlang::nif::error(env, "enif_alloc failed");
        return false;
    }

    ERL_NIF_TERM default_size_term;
    if (_fb_var_to_erl(env, fb_var->default_size(), default_size_term)) {
        enif_make_map_put(env, out, erlang::nif::atom(env, "default_size"), default_size_term, &out);
    }

    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::BoundingBoxProperties* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    std::map<std::string, std::string> properties;
    properties["type"] = EnumNameBoundingBoxType(fb_var->type());
    properties["coordinate_type"] = EnumNameCoordinateType(fb_var->coordinate_type());
    if (erlang::nif::make(env, properties, out, true)) {
        out = erlang::nif::error(env, "enif_alloc failed");
        return false;
    }

    ERL_NIF_TERM index_term;
    const flatbuffers::Vector<uint32_t> * index = fb_var->index();
    if (index) {
        if (erlang::nif::make_u32_list_from_c_array(env, index->size(), index->data(), index_term)) {
            out = index_term;
            return false;
        }
        enif_make_map_put(env, out, erlang::nif::atom(env, "index"), index_term, &out);
    }

    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::AudioProperties* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    ERL_NIF_TERM keys[2], values[2];
    keys[0] = erlang::nif::atom(env, "sample_rate");
    keys[1] = erlang::nif::atom(env, "channels");
    values[0] = erlang::nif::make(env, fb_var->sample_rate());
    values[1] = erlang::nif::make(env, fb_var->channels());

    if (!enif_make_map_from_arrays(env, keys, values, 2, &out)) {
        out = erlang::nif::error(env, "duplicate keys");
        return false;
    }

    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::NormalizationOptions* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    ERL_NIF_TERM keys[2], values[2];

    size_t items = 0;
    auto mean = fb_var->mean();
    if (mean) {
        ERL_NIF_TERM mean_term;
        if (erlang::nif::make_f64_list_from_c_array(env, mean->size(), mean->data(), mean_term)) {
            out = mean_term;
            return false;
        }
        keys[items] = erlang::nif::atom(env, "mean");
        values[items] = mean_term;
        items++;
    }

    auto std = fb_var->std();
    if (std) {
        ERL_NIF_TERM std_term;
        if (erlang::nif::make_f64_list_from_c_array(env, std->size(), std->data(), std_term)) {
            out = std_term;
            return false;
        }
        keys[items] = erlang::nif::atom(env, "std");
        values[items] = std_term;
        items++;
    }

    if (!enif_make_map_from_arrays(env, keys, values, items, &out)) {
        out = erlang::nif::error(env, "duplicate keys");
        return false;
    }

    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::ScoreCalibrationOptions* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    std::map<std::string, std::string> options;
    options["score_transformation"] = EnumNameScoreTransformationType(fb_var->score_transformation());
    if (erlang::nif::make(env, options, out, true)) {
        out = erlang::nif::error(env, "enif_alloc failed");
        return false;
    }

    enif_make_map_put(env, out, erlang::nif::atom(env, "default_score"), erlang::nif::make(env, fb_var->default_score()), &out);

    return false;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::ScoreThresholdingOptions* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    ERL_NIF_TERM keys[1], values[1];
    keys[0] = erlang::nif::atom(env, "default_score");
    values[0] = erlang::nif::make(env, fb_var->global_score_threshold());
    enif_make_map_from_arrays(env, keys, values, 1, &out);

    return false;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::BertTokenizerOptions* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    ERL_NIF_TERM keys[1], values[1];
    if (!_fb_var_to_erl(env, fb_var->vocab_file(), values[0])) {
        out = values[0];
        return false;
    }

    keys[0] = erlang::nif::atom(env, "vocab_file");
    enif_make_map_from_arrays(env, keys, values, 1, &out);

    return false;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::SentencePieceTokenizerOptions* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    ERL_NIF_TERM keys[2], values[2];
    if (!_fb_var_to_erl(env, fb_var->sentencePiece_model(), values[0])) {
        out = values[0];
        return false;
    }
    keys[0] = erlang::nif::atom(env, "sentencePiece_model");

    if (!_fb_var_to_erl(env, fb_var->vocab_file(), values[1])) {
        out = values[1];
        return false;
    }
    keys[1] = erlang::nif::atom(env, "vocab_file");

    enif_make_map_from_arrays(env, keys, values, 2, &out);

    return false;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::RegexTokenizerOptions* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    ERL_NIF_TERM keys[2], values[2];
    if (!_fb_var_to_erl(env, fb_var->delim_regex_pattern(), values[0])) {
        out = values[0];
        return false;
    }
    keys[0] = erlang::nif::atom(env, "delim_regex_pattern");

    if (!_fb_var_to_erl(env, fb_var->vocab_file(), values[1])) {
        out = values[1];
        return false;
    }
    keys[1] = erlang::nif::atom(env, "vocab_file");

    enif_make_map_from_arrays(env, keys, values, 2, &out);

    return false;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::ValueRange* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    ERL_NIF_TERM keys[2], values[2];
    keys[0] = erlang::nif::atom(env, "min");
    keys[1] = erlang::nif::atom(env, "max");
    values[0] = erlang::nif::make(env, fb_var->min());
    values[1] = erlang::nif::make(env, fb_var->max());

    if (!enif_make_map_from_arrays(env, keys, values, 2, &out)) {
        out = erlang::nif::error(env, "duplicate keys");
        return false;
    }

    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::Content* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    std::map<std::string, std::string> content;
    tflite::ContentProperties content_properties_type = fb_var->content_properties_type();
    if (content_properties_type == tflite::ContentProperties::ContentProperties_NONE) {
        return false;
    }

    ERL_NIF_TERM content_properties_term;
    bool ok;
    switch (content_properties_type) {
        case tflite::ContentProperties::ContentProperties_FeatureProperties:
            ok = _fb_var_to_erl(env, fb_var->content_properties_as_FeatureProperties(), content_properties_term);
            break;
        case tflite::ContentProperties::ContentProperties_ImageProperties:
            ok = _fb_var_to_erl(env, fb_var->content_properties_as_ImageProperties(), content_properties_term);
            break;
        case tflite::ContentProperties::ContentProperties_BoundingBoxProperties:
            ok = _fb_var_to_erl(env, fb_var->content_properties_as_BoundingBoxProperties(), content_properties_term);
            break;
        case tflite::ContentProperties::ContentProperties_AudioProperties:
            ok = _fb_var_to_erl(env, fb_var->content_properties_as_AudioProperties(), content_properties_term);
            break;
        default:
            return false;
    }

    if (!ok) {
        out = content_properties_term;
        return false;
    }

    content["content_properties_type"] = EnumNameContentProperties(content_properties_type);
    if (erlang::nif::make(env, content, out, true)) {
        out = erlang::nif::error(env, "enif_alloc failed");
        return false;
    }

    enif_make_map_put(env, out, erlang::nif::atom(env, "content_properties"), content_properties_term, &out);

    ERL_NIF_TERM range_term;
    if (_fb_var_to_erl(env, fb_var->range(), range_term)) {
        enif_make_map_put(env, out, erlang::nif::atom(env, "range"), range_term, &out);
    }

    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const flatbuffers::Vector<flatbuffers::Offset<tflite::ProcessUnit>>* fb_vars, ERL_NIF_TERM &out) {
    if (!fb_vars) {
        return false;
    }

    size_t count = fb_vars->size();
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return true;
    }

    ERL_NIF_TERM * process_units_ = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * count);
    if (!process_units_) {
        out = erlang::nif::error(env, "enif_alloc failed");
        return false;
    }

    size_t valid = 0;
    for (size_t i = 0; i < count; i++) {
        auto process_unit = fb_vars->Get(i);
        if (process_unit && process_unit->options_type() != tflite::ProcessUnitOptions::ProcessUnitOptions_NONE) {
            std::map<std::string, std::string> process_unit_;
            tflite::ProcessUnitOptions options_type = process_unit->options_type();
            process_unit_["options_type"] = EnumNameProcessUnitOptions(options_type);

            ERL_NIF_TERM options_term;
            bool ok;
            switch (options_type) {
                case tflite::ProcessUnitOptions_NormalizationOptions:
                    ok = _fb_var_to_erl(env, process_unit->options_as_NormalizationOptions(), options_term);
                    break;
                case tflite::ProcessUnitOptions_ScoreCalibrationOptions:
                    ok = _fb_var_to_erl(env, process_unit->options_as_ScoreCalibrationOptions(), options_term);
                    break;
                case tflite::ProcessUnitOptions_ScoreThresholdingOptions:
                    ok = _fb_var_to_erl(env, process_unit->options_as_ScoreThresholdingOptions(), options_term);
                    break;
                case tflite::ProcessUnitOptions_BertTokenizerOptions:
                    ok = _fb_var_to_erl(env, process_unit->options_as_BertTokenizerOptions(), options_term);
                    break;
                case tflite::ProcessUnitOptions_SentencePieceTokenizerOptions:
                    ok = _fb_var_to_erl(env, process_unit->options_as_SentencePieceTokenizerOptions(), options_term);
                    break;
                case tflite::ProcessUnitOptions_RegexTokenizerOptions:
                    ok = _fb_var_to_erl(env, process_unit->options_as_RegexTokenizerOptions(), options_term);
                    break;
                default:
                    return false;
            }

            if (!ok) {
                enif_free(process_units_);
                out = options_term;
                return false;
            }

            if (erlang::nif::make(env, process_unit_, process_units_[valid], true)) {
                enif_free(process_units_);
                out = erlang::nif::error(env, "enif_alloc failed");
                return false;
            }

            enif_make_map_put(env, process_units_[valid], erlang::nif::atom(env, "options"), options_term, &process_units_[valid]);

            valid++;
        }
    }

    out = enif_make_list_from_array(env, process_units_, valid);
    enif_free(process_units_);

    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const flatbuffers::Vector<flatbuffers::Offset<tflite::TensorMetadata>>* fb_vars, ERL_NIF_TERM &out) {
    if (!fb_vars) {
        return false;
    }

    size_t count = fb_vars->size();
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return true;
    }

    ERL_NIF_TERM * tensor_metadata_v = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * count);
    if (!tensor_metadata_v) {
        out = erlang::nif::error(env, "enif_alloc failed");
        return false;
    }

    size_t valid = 0;
    for (size_t i = 0; i < count; i++) {
        auto tensor_metadata = fb_vars->Get(i);
        if (tensor_metadata) {
            std::map<std::string, std::string> tensor_metadata_;
            _tflite_metadata_set_key_value(tensor_metadata_, "name", tensor_metadata->name());
            _tflite_metadata_set_key_value(tensor_metadata_, "description", tensor_metadata->description());

            if (erlang::nif::make(env, tensor_metadata_, tensor_metadata_v[valid], true)) {
                enif_free(tensor_metadata_v);
                out = erlang::nif::error(env, "enif_alloc failed");
                return false;
            }

            ERL_NIF_TERM dimension_names_term;
            if (_fb_var_to_erl(env, tensor_metadata->dimension_names(), dimension_names_term)) {
                enif_make_map_put(env, tensor_metadata_v[valid], erlang::nif::atom(env, "dimension_names"), dimension_names_term, &tensor_metadata_v[valid]);
            }

            ERL_NIF_TERM content_term;
            if (_fb_var_to_erl(env, tensor_metadata->content(), content_term)) {
                enif_make_map_put(env, tensor_metadata_v[valid], erlang::nif::atom(env, "content"), content_term, &tensor_metadata_v[valid]);
            }

            ERL_NIF_TERM process_units_term;
            if (_fb_var_to_erl(env, tensor_metadata->process_units(), process_units_term)) {
                enif_make_map_put(env, tensor_metadata_v[valid], erlang::nif::atom(env, "process_units"), process_units_term, &tensor_metadata_v[valid]);
            }

            ERL_NIF_TERM stats_term;
            if (_fb_var_to_erl(env, tensor_metadata->stats(), stats_term)) {
                enif_make_map_put(env, tensor_metadata_v[valid], erlang::nif::atom(env, "stats"), stats_term, &tensor_metadata_v[valid]);
            }

            ERL_NIF_TERM associated_files_term;
            if (_fb_var_to_erl(env, tensor_metadata->associated_files(), associated_files_term)) {
                enif_make_map_put(env, tensor_metadata_v[valid], erlang::nif::atom(env, "associated_files"), associated_files_term, &tensor_metadata_v[valid]);
            }

            valid++;
        }
    }

    out = enif_make_list_from_array(env, tensor_metadata_v, valid);
    enif_free(tensor_metadata_v);
    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const flatbuffers::Vector<flatbuffers::Offset<tflite::TensorGroup>>* fb_vars, ERL_NIF_TERM &out) {
    if (!fb_vars) {
        return false;
    }

    size_t count = fb_vars->size();
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return true;
    }

    ERL_NIF_TERM * tensor_group_ = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * count);
    if (!tensor_group_) {
        out = erlang::nif::error(env, "enif_alloc failed");
        return false;
    }

    size_t valid = 0;
    for (size_t i = 0; i < count; i++) {
        auto tensor_group = fb_vars->Get(i);
        if (tensor_group) {
            std::map<std::string, std::string> tensor_group_map;
            _tflite_metadata_set_key_value(tensor_group_map, "name", tensor_group->name());

            if (erlang::nif::make(env, tensor_group_map, tensor_group_[valid], true)) {
                enif_free(tensor_group_);
                out = erlang::nif::error(env, "enif_alloc failed");
                return false;
            }

            ERL_NIF_TERM tensor_names_term;
            if (_fb_var_to_erl(env, tensor_group->tensor_names(), tensor_names_term)) {
                enif_make_map_put(env, tensor_group_[valid], erlang::nif::atom(env, "tensor_names"), tensor_names_term, &tensor_group_[valid]);
            }

            valid++;
        }
    }

    out = enif_make_list_from_array(env, tensor_group_, valid);
    enif_free(tensor_group_);
    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const flatbuffers::Vector<flatbuffers::Offset<tflite::SubGraphMetadata>>* fb_vars, ERL_NIF_TERM &out) {
    if (!fb_vars) {
        return false;
    }

    size_t count = fb_vars->size();
    if (count == 0) {
        out = enif_make_list_from_array(env, nullptr, 0);
        return true;
    }

    ERL_NIF_TERM * subgraphs = (ERL_NIF_TERM *)enif_alloc(sizeof(ERL_NIF_TERM) * count);
    if (!subgraphs) {
        out = erlang::nif::error(env, "enif_alloc failed");
        return false;
    }

    size_t valid = 0;
    for (size_t i = 0; i < count; i++) {
        auto subgraph = fb_vars->Get(i);
        if (subgraph) {
            std::map<std::string, std::string> metadata_;
            _tflite_metadata_set_key_value(metadata_, "name", subgraph->name());
            _tflite_metadata_set_key_value(metadata_, "description", subgraph->description());

            if (erlang::nif::make(env, metadata_, subgraphs[valid], true)) {
                enif_free(subgraphs);
                out = erlang::nif::error(env, "enif_alloc failed");
                return false;
            }

            ERL_NIF_TERM input_tensor_metadata_term;
            if (_fb_var_to_erl(env, subgraph->input_tensor_metadata(), input_tensor_metadata_term)) {
                enif_make_map_put(env, subgraphs[valid], erlang::nif::atom(env, "input_tensor_metadata"), input_tensor_metadata_term, &subgraphs[valid]);
            }

            ERL_NIF_TERM output_tensor_metadata_term;
            if (_fb_var_to_erl(env, subgraph->output_tensor_metadata(), output_tensor_metadata_term)) {
                enif_make_map_put(env, subgraphs[valid], erlang::nif::atom(env, "output_tensor_metadata"), output_tensor_metadata_term, &subgraphs[valid]);
            }

            ERL_NIF_TERM associated_files_term;
            if (_fb_var_to_erl(env, subgraph->associated_files(), associated_files_term)) {
                enif_make_map_put(env, subgraphs[valid], erlang::nif::atom(env, "associated_files"), associated_files_term, &subgraphs[valid]);
            }

            ERL_NIF_TERM input_process_units_term;
            if (_fb_var_to_erl(env, subgraph->input_process_units(), input_process_units_term)) {
                enif_make_map_put(env, subgraphs[valid], erlang::nif::atom(env, "input_process_units"), input_process_units_term, &subgraphs[valid]);
            }

            ERL_NIF_TERM output_process_units_term;
            if (_fb_var_to_erl(env, subgraph->output_process_units(), output_process_units_term)) {
                enif_make_map_put(env, subgraphs[valid], erlang::nif::atom(env, "output_process_units"), output_process_units_term, &subgraphs[valid]);
            }

            ERL_NIF_TERM input_tensor_groups_term;
            if (_fb_var_to_erl(env, subgraph->input_tensor_groups(), input_tensor_groups_term)) {
                enif_make_map_put(env, subgraphs[valid], erlang::nif::atom(env, "input_tensor_groups"), input_tensor_groups_term, &subgraphs[valid]);
            }

            ERL_NIF_TERM output_tensor_groups_term;
            if (_fb_var_to_erl(env, subgraph->output_tensor_groups(), output_tensor_groups_term)) {
                enif_make_map_put(env, subgraphs[valid], erlang::nif::atom(env, "output_tensor_groups"), output_tensor_groups_term, &subgraphs[valid]);
            }

            valid++;
        }
    }

    out = enif_make_list_from_array(env, subgraphs, valid);
    enif_free(subgraphs);
    return true;
}

template <>
bool _fb_var_to_erl(ErlNifEnv *env, const tflite::ModelMetadata* fb_var, ERL_NIF_TERM &out) {
    if (!fb_var) {
        return false;
    }

    std::map<std::string, std::string> metadata_;
    _tflite_metadata_set_key_value(metadata_, "name", fb_var->name());
    _tflite_metadata_set_key_value(metadata_, "description", fb_var->description());
    _tflite_metadata_set_key_value(metadata_, "version", fb_var->version());
    _tflite_metadata_set_key_value(metadata_, "author", fb_var->author());
    _tflite_metadata_set_key_value(metadata_, "license", fb_var->license());
    _tflite_metadata_set_key_value(metadata_, "min_parser_version", fb_var->min_parser_version());

    if (erlang::nif::make(env, metadata_, out, true)) {
        return false;
    }

    ERL_NIF_TERM associated_files_term;
    if (_fb_var_to_erl(env, fb_var->associated_files(), associated_files_term)) {
        enif_make_map_put(env, out, erlang::nif::atom(env, "associated_files"), associated_files_term, &out);
    }

    ERL_NIF_TERM subgraph_metadata_term;
    if (_fb_var_to_erl(env, fb_var->subgraph_metadata(), subgraph_metadata_term)) {
        enif_make_map_put(env, out, erlang::nif::atom(env, "subgraph_metadata"), subgraph_metadata_term, &out);
    }

    return true;
}

ERL_NIF_TERM tflite_metadata_to_erl_term(ErlNifEnv *env, const void *buf) {
    const tflite::ModelMetadata * metadata = nullptr;
    if (!buf || !(metadata = tflite::GetModelMetadata(buf))) {
        return erlang::nif::atom(env, "nil");
    }

    ERL_NIF_TERM out;
    _fb_var_to_erl(env, metadata, out);
    return out;
}

void _tflite_metadata_set_key_value(std::map<std::string, std::string>& map, const std::string& key, const flatbuffers::String * value) {
    if (value) {
        map[key] = value->str();
    }
}
