#!/usr/bin/env bash
# Builds the NIF under a sanitizer and drives it, because the defects these
# catch are invisible to the test suite: a data race that TSan reports in three
# runs out of three passed 155 Common Test cases without a murmur.
#
#   scripts/run_sanitizer.sh thread     # data races
#   scripts/run_sanitizer.sh address    # use after free, double free, overflow
#
# Going through `erl` does not work. macOS strips DYLD_* when it execs a
# SIP-protected binary, and erl is a shell script, so the preload is gone before
# the emulator starts and the interceptors never install. beam.smp is exec'd
# directly for that reason.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

KIND="${1:-thread}"
case "$KIND" in
  thread)  RT=tsan; PATTERN="WARNING: ThreadSanitizer"
           export TSAN_OPTIONS="exitcode=0:halt_on_error=0" ;;
  address) RT=asan; PATTERN="ERROR: AddressSanitizer"
           export ASAN_OPTIONS="detect_leaks=0:halt_on_error=0" ;;
  *) echo "usage: $0 [thread|address]" >&2; exit 2 ;;
esac

RUNTIME=$(ls /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/*/lib/darwin/libclang_rt.${RT}_osx_dynamic.dylib 2>/dev/null | head -1 || true)
if [ -z "$RUNTIME" ]; then
  echo "no ${RT} runtime found; this script is macOS only for now" >&2
  exit 1
fi

ERTS=$(erl -noshell -eval 'io:format("~ts/erts-~ts",[code:root_dir(),erlang:system_info(version)]),halt().')
ERTS_INCLUDE_DIR="$ERTS/include"
export ERTS_INCLUDE_DIR TFLITE_BEAM_PREFER_PRECOMPILED=false

echo "building with -fsanitize=${KIND}"
export CMAKE_OPTIONS="${CMAKE_OPTIONS:-} -DCMAKE_POLICY_VERSION_MINIMUM=3.5 -DCMAKE_BUILD_TYPE=RelWithDebInfo \
  -DTFLITE_BEAM_ENABLE_LITERT_API=ON -DLITERT_ENABLE_GPU=ON \
  -DCMAKE_C_FLAGS=-fsanitize=${KIND} -DCMAKE_CXX_FLAGS=-fsanitize=${KIND} \
  -DCMAKE_SHARED_LINKER_FLAGS=-fsanitize=${KIND} -DCMAKE_EXE_LINKER_FLAGS=-fsanitize=${KIND}"
rm -rf _build/default/lib/tflite_beam/cmake_tflite_beam priv/tflite_beam.so
make TFLITE_BEAM_MAKE=make >/dev/null
codesign -s - -f priv/tflite_beam.so >/dev/null 2>&1 || true
rebar3 compile >/dev/null

DRIVE="${ROOT}/scripts/sanitizer_drive.erl"
OUT=$(mktemp -d)
erlc -o "$OUT" -I "${ROOT}/src" "$DRIVE"

echo "driving"
LOG="$OUT/sanitizer.log"
set +e
BINDIR="$ERTS/bin" DYLD_INSERT_LIBRARIES="$RUNTIME" TFLITE_BEAM_ENABLE_FAULT_INJECTION=1 \
  "$ERTS/bin/beam.smp" -- -root "$(erl -noshell -eval 'io:format("~ts",[code:root_dir()]),halt().')" \
  -bindir "$ERTS/bin" -progname erl -- -home "$HOME" -- -noshell \
  -pa "${ROOT}/_build/default/lib/tflite_beam/ebin" -pa "$OUT" \
  -root_dir "$ROOT" -run sanitizer_drive main > "$LOG" 2>&1
set -e

# A drive that crashed reports nothing, which looks exactly like a clean run.
# Only its own last line says otherwise, so that is what is checked first.
if ! grep -q "sanitizer drive complete" "$LOG"; then
  echo "the drive did not finish, so nothing was exercised:" >&2
  tail -20 "$LOG" >&2
  exit 1
fi

COUNT=$(grep -c "$PATTERN" "$LOG" || true)
echo "${KIND}: ${COUNT} report(s)"
if [ "$COUNT" != "0" ]; then
  grep -A12 "$PATTERN" "$LOG" | head -40
  exit 1
fi
tail -3 "$LOG"
