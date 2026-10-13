#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FLATBUFFERS="${ROOT}/_build/default/lib/tflite_beam/cmake_tflite_beam/flatbuffers/include"
TENSORFLOW="$(find "${ROOT}/3rd_party/tensorflow" -maxdepth 1 -name 'tensorflow-*' | sort -V | tail -1)"
BIN="$(mktemp -t make_graph_fixture)"
trap 'rm -- "$BIN"' EXIT

c++ -std=c++17 -I "$FLATBUFFERS" -I "$TENSORFLOW" \
    "${ROOT}/scripts/make_graph_fixture.cpp" -o "$BIN"
"$BIN" "${ROOT}/test/models/graph_options.tflite"
"$BIN" "${ROOT}/test/models/graph_options_mismatched.tflite" mismatched
"$BIN" "${ROOT}/test/models/graph_options_split_mismatched.tflite" split_mismatched
"$BIN" "${ROOT}/test/models/graph_options_composite_mismatched.tflite" composite_mismatched
"$BIN" "${ROOT}/test/models/graph_options_missing.tflite" missing
