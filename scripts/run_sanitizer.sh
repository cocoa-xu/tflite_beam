#!/usr/bin/env bash
# Builds the NIF under a sanitizer and drives it, because the defects these
# catch are invisible to the test suite: a data race that TSan reports in three
# runs out of three passed 155 Common Test cases without a murmur.
#
#   scripts/run_sanitizer.sh thread     # data races
#   scripts/run_sanitizer.sh address    # use after free, double free, overflow
#
# macOS and Linux both work. The runtime comes from Xcode on one and from the
# compiler's own -print-file-name on the other.
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
           # ignore_noninstrumented_modules defaults to 1 on macOS and 0 on
           # Linux, and that single difference is the whole of why this ran
           # clean on one and not the other. beam.smp is not instrumented, so
           # anything TSan says about a race inside it is guesswork; without
           # this, a Linux run reports erts internals it cannot see the
           # synchronisation for. It does not hide anything in our own code:
           # a race is still reported when either access is instrumented.
           #
           # detect_deadlocks=0 because TSan's detector aborts on the emulator:
           # "sanitizer_deadlock_detector.h:67 n_all_locks_ < ..." is its own
           # fixed-size table overflowing on how many locks the BEAM holds.
           export TSAN_OPTIONS="exitcode=0:halt_on_error=0:ignore_noninstrumented_modules=1:detect_deadlocks=0" ;;
  address) RT=asan; PATTERN="ERROR: AddressSanitizer"
           export ASAN_OPTIONS="detect_leaks=0:halt_on_error=0" ;;
  *) echo "usage: $0 [thread|address]" >&2; exit 2 ;;
esac

# The runtime has to be loaded before the emulator starts, on both systems and
# for the same reason: the NIF arrives through dlopen, and by then it is too
# late to install interceptors. What differs is how it gets there.
case "$(uname -s)" in
  Darwin)
    RUNTIME=$(ls /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/*/lib/darwin/libclang_rt.${RT}_osx_dynamic.dylib 2>/dev/null | head -1 || true)
    ;;
  Linux)
    # clang, not gcc. XNNPACK writes XNN_OOB_READS after the declarator, and
    # under a sanitizer that macro expands to __attribute__((__no_sanitize__)).
    # GCC rejects an attribute there in a function definition and clang accepts
    # it, so a gcc run dies on f16-dwconv-25p8c-minmax-fma3-acc2.c and about a
    # hundred of its siblings, with a message that says nothing about
    # sanitizers. Verified both ways on gcc 15.2 and clang 21.1 on 2026-08-29.
    if [ -z "${CC:-}" ] && [ -z "${CXX:-}" ]; then
      if command -v clang >/dev/null 2>&1 && command -v clang++ >/dev/null 2>&1; then
        export CC=clang CXX=clang++
      else
        echo "the sanitizers need clang on Linux: XNNPACK's XNN_OOB_READS placement" >&2
        echo "is rejected by gcc under -fsanitize. Install clang, or set CC/CXX yourself." >&2
        exit 1
      fi
    fi
    # clang and gcc name their runtimes differently, and asking clang for
    # "libtsan.so" is worse than getting nothing: on Ubuntu it falls back to
    # gcc'"'"'s search path and hands back gcc'"'"'s libtsan, which preloaded against a
    # clang-built library segfaults the emulator before it prints anything.
    # Ask for the clang name first, and only then the gcc one.
    ARCH=$(uname -m)
    RUNTIME=""
    for LIBNAME in "libclang_rt.${RT}-${ARCH}.so" "lib${RT}.so"; do
      CANDIDATE=$("${CXX:-c++}" -print-file-name="$LIBNAME" 2>/dev/null || true)
      # -print-file-name echoes the name back unchanged when it cannot find it
      if [ "$CANDIDATE" != "$LIBNAME" ] && [ -f "$CANDIDATE" ]; then
        RUNTIME="$CANDIDATE"
        break
      fi
    done
    ;;
  *)
    echo "no sanitizer support for $(uname -s)" >&2; exit 1 ;;
esac
if [ -z "$RUNTIME" ] || [ ! -f "$RUNTIME" ]; then
  echo "no ${RT} runtime found for $(uname -s); install it or point CXX at a compiler that ships one" >&2
  exit 1
fi

ERTS=$(erl -noshell -eval 'io:format("~ts/erts-~ts",[code:root_dir(),erlang:system_info(version)]),halt().')
ERTS_INCLUDE_DIR="$ERTS/include"

ERL_ROOT=$(erl -noshell -eval 'io:format("~ts",[code:root_dir()]),halt().')

# One way to start an emulator with the sanitizer runtime inside it, because
# there are two of them and getting the wrong one is silent. macOS strips DYLD_*
# when it execs a SIP-protected binary and erl is a shell script, so the preload
# is gone before the emulator starts; beam.smp is exec'd directly to get around
# that. Linux does no such stripping, so erl with LD_PRELOAD is enough.
#
# This lives in one function because it did not, once: the Linux branch was
# added to the runtime lookup and silently not to the drive, so every Linux run
# started an uninstrumented emulator, every NIF call raised undef, and the
# script looked broken while the same command by hand worked.
run_beam() {
  if [ "$(uname -s)" = "Darwin" ]; then
    BINDIR="$ERTS/bin" DYLD_INSERT_LIBRARIES="$RUNTIME" \
      "$ERTS/bin/beam.smp" -- -root "$ERL_ROOT" \
      -bindir "$ERTS/bin" -progname erl -- -home "$HOME" -- -noshell "$@"
  else
    LD_PRELOAD="$RUNTIME" erl -noshell "$@"
  fi
}

export ERTS_INCLUDE_DIR TFLITE_BEAM_PREFER_PRECOMPILED=false

echo "building with -fsanitize=${KIND} using ${CXX:-c++}"
echo "preloading ${RUNTIME}"
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

# A NIF that will not load makes every call in the drive raise undef, and the
# emulator's on_load warning is lost when -run fails during boot, so the whole
# thing reads as a mystery. Ask once, up front, and say what went wrong.
if [ ! -f "priv/${LIBNAME_SO:-tflite_beam.so}" ]; then
  echo "priv/tflite_beam.so is missing after the build; nothing to drive" >&2
  exit 1
fi
LOADCHECK=$(run_beam \
  -pa "${ROOT}/_build/default/lib/tflite_beam/ebin" \
  -eval 'io:format("~p", [code:load_file(tflite_beam_nif)]), halt(0).' 2>&1 | tail -1)
case "$LOADCHECK" in
  *"{module,tflite_beam_nif}"*) ;;
  *) echo "the NIF will not load, so the drive would only report undef:" >&2
     echo "  $LOADCHECK" >&2
     exit 1 ;;
esac

echo "driving"
LOG="$OUT/sanitizer.log"
set +e
TFLITE_BEAM_ENABLE_FAULT_INJECTION=1 run_beam \
  -pa "${ROOT}/_build/default/lib/tflite_beam/ebin" -pa "$OUT" \
  -root_dir "$ROOT" -run sanitizer_drive main > "$LOG" 2>&1
set -e

COUNT=$(grep -c "$PATTERN" "$LOG" || true)

# A drive that crashed reports nothing, which looks exactly like a clean run.
# Only its own last line says otherwise, so that is what is checked. The count
# is taken first and printed here because a real race can crash the drive, and
# that is exactly when its findings are worth reading rather than discarding.
if ! grep -q "sanitizer drive complete" "$LOG"; then
  echo "the drive did not finish, so what follows is partial at best" >&2
  echo "${KIND}: ${COUNT} report(s) before it stopped" >&2
  if [ "$COUNT" != "0" ]; then grep -A12 "$PATTERN" "$LOG" >&2; fi
  echo "--- how it ended ---" >&2
  tail -20 "$LOG" >&2
  exit 1
fi

echo "${KIND}: ${COUNT} report(s)"
if [ "$COUNT" != "0" ]; then
  grep -A12 "$PATTERN" "$LOG" | head -40
  exit 1
fi
tail -3 "$LOG"
