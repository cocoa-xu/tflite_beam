#!/usr/bin/env bash
# Regenerates test/models/metadata_corners.bin. The output is committed, so this
# only needs running when the fixture has to cover something new.
#
# It needs a configured build tree, because it borrows flatbuffers' headers and
# TfLite's model schema from there.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FLATBUFFERS="${ROOT}/_build/default/lib/tflite_beam/cmake_tflite_beam/flatbuffers/include"
TENSORFLOW="$(find "${ROOT}/3rd_party/tensorflow" -maxdepth 1 -name 'tensorflow-*' | sort -V | tail -1)"

if [ ! -d "${FLATBUFFERS}" ]; then
  echo "no flatbuffers headers at ${FLATBUFFERS}; build the NIF first" >&2
  exit 1
fi

OUT="${ROOT}/test/models/metadata_corners.bin"
BIN="$(mktemp -t make_metadata_fixture)"
trap 'rm -f "${BIN}"' EXIT

c++ -std=c++17 \
  -I "${FLATBUFFERS}" \
  -I "${ROOT}/c_src" \
  -I "${TENSORFLOW}" \
  -o "${BIN}" "${ROOT}/scripts/make_metadata_fixture.cpp"

"${BIN}" "${OUT}"
