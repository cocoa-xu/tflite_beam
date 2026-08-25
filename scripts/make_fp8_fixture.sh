#!/usr/bin/env bash
# Regenerates test/models/fp8_types.bin. The output is committed, so this
# only needs running when the two 8 bit float types need re-emitting.
#
# It needs a configured build tree, because it borrows flatbuffers' headers and
# TfLite's model schema from there.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FLATBUFFERS="${ROOT}/_build/default/lib/tflite_beam/cmake_tflite_beam/flatbuffers/include"
LITERT="$(find "${ROOT}/3rd_party/litert" -maxdepth 1 -name 'LiteRT-*' | sort -V | tail -1)"

if [ ! -d "${FLATBUFFERS}" ]; then
  echo "no flatbuffers headers at ${FLATBUFFERS}; build the NIF first" >&2
  exit 1
fi

OUT="${ROOT}/test/models/fp8_types.bin"
BIN="$(mktemp -t make_fp8_fixture)"
trap 'rm -f "${BIN}"' EXIT

c++ -std=c++17 \
  -I "${FLATBUFFERS}" \
  -I "${LITERT}" \
  -o "${BIN}" "${ROOT}/scripts/make_fp8_fixture.cpp"

"${BIN}" "${OUT}"
