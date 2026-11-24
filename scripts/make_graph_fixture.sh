#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FLATBUFFERS="${ROOT}/_build/default/lib/tflite_beam/cmake_tflite_beam/flatbuffers/include"
LITERT="$(find "${ROOT}/3rd_party/litert" -maxdepth 1 -name 'LiteRT-*' | sort -V | tail -1)"
BIN="$(mktemp -t make_graph_fixture)"
trap 'rm -- "$BIN"' EXIT

c++ -std=c++17 -I "$FLATBUFFERS" -I "$LITERT" \
    "${ROOT}/scripts/make_graph_fixture.cpp" -o "$BIN"
"$BIN" "${ROOT}/test/models/graph_options.tflite"
"$BIN" "${ROOT}/test/models/graph_options_mismatched.tflite" mismatched
"$BIN" "${ROOT}/test/models/graph_options_split_mismatched.tflite" split_mismatched
"$BIN" "${ROOT}/test/models/graph_options_composite_mismatched.tflite" composite_mismatched
"$BIN" "${ROOT}/test/models/graph_options_conv3d_transpose_mismatched.tflite" conv3d_transpose_mismatched
"$BIN" "${ROOT}/test/models/graph_options_lrn_mismatched.tflite" lrn_mismatched
"$BIN" "${ROOT}/test/models/graph_options_missing.tflite" missing
