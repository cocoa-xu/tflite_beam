#!/bin/bash

LIBEDGETPU_RUNTIME_PRIV="$1"
TFLITE_BEAM_CORAL_LIBEDGETPU_UNARCHIVED_DIR="$2"
TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET="$3"
TFLITE_BEAM_CORAL_USB_THROTTLE="$4"
TFLITE_BEAM_CORAL_LIBEDGETPU_URL="$5"
TFLITE_BEAM_CORAL_LIBEDGETPU_RUNTIME="$6"
TFLITE_BEAM_CACHE_DIR="$7"
TFLITE_BEAM_COMPILE_WITH_REBAR="$8"

LIBEDGETPU_VERSION="0.1.6"
LIBEDGETPU_BASE_URL="https://github.com/cocoa-xu/libedgetpu/releases/download/v${LIBEDGETPU_VERSION}"

get_libedgetpu_triplet() {
  if [ "${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}" = "native" ]; then
    if [ "${TFLITE_BEAM_COMPILE_WITH_REBAR}" = "true" ]; then
      if [[ -n "${TARGET_ARCH}" && -n "${TARGET_OS}" && -n "${TARGET_ABI}" ]]; then
        if [ "${TARGET_ARCH}" = "arm" ]; then
          case "${TARGET_CPU}" in
            arm1176jzf_s*)
              echo "armv6-${TARGET_OS}-${TARGET_ABI}"
            ;;
            cortex*)
              echo "armv7l-${TARGET_OS}-${TARGET_ABI}"
            ;;
            *)
              echo "Unknown TARGET_CPU: ${TARGET_CPU}"
              exit 1
            ;;
          esac
        else
          echo "${TARGET_ARCH}-${TARGET_OS}-${TARGET_ABI}"
        fi
      fi
    fi

    UNAME_M="$(uname -m)"
    if [ -n "${TARGET_ARCH}" ]; then
      UNAME_M="${TARGET_ARCH}"
    fi
    UNAME_S="$(uname -s)"
    if [ -n "${TARGET_OS}" ]; then
      UNAME_S="${TARGET_OS}"
    fi

    case "${UNAME_M}-${UNAME_S}" in
      arm64-Darwin*)
        echo "aarch64-apple-darwin"
      ;;
      x86_64-Darwin*)
        echo "x86_64-apple-darwin"
      ;;
      *-Linux*)
        # check libc type
        ABI="gnu"

        if [ -n "${TARGET_ABI}" ]; then
          ABI="${TARGET_ABI}"
        else
          if [ -x "$(which ldd)" ]; then
            ldd --version | grep musl >/dev/null ;
            if [ $? -eq 0 ]; then
              ABI="musl"
            fi
          fi

          case "${UNAME_M}" in
            armv6*|armv7*)
              ABI="${ABI}eabihf"
            ;;
          esac
        fi

        echo "${UNAME_M}-linux-${ABI}"
      ;;
    esac
  else
    echo "${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}"
  fi
}

echo "TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET: ${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}"

if [ "${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}" = "native" ]; then
  TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET="$(get_libedgetpu_triplet)"
  TFLITE_BEAM_CORAL_LIBEDGETPU_RUNTIME="edgetpu_runtime_${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}_v${LIBEDGETPU_VERSION}"
  TFLITE_BEAM_CORAL_LIBEDGETPU_URL="${LIBEDGETPU_BASE_URL}/${TFLITE_BEAM_CORAL_LIBEDGETPU_RUNTIME}.tar.gz"
fi
LIBEDGETPU_UNARCHIVED_DIR="${TFLITE_BEAM_CACHE_DIR}/${TFLITE_BEAM_CORAL_LIBEDGETPU_RUNTIME}"
LIBEDGETPU_ARCHIVE="${TFLITE_BEAM_CACHE_DIR}/${TFLITE_BEAM_CORAL_LIBEDGETPU_RUNTIME}.tar.gz"

download_libedgetpu() {
  if [ ! -e "${LIBEDGETPU_ARCHIVE}" ]; then
    echo "Downloading libedgetpu ${TFLITE_BEAM_CORAL_LIBEDGETPU_URL}..."
    if [ -e "$(which curl)" ]; then
      curl -fSL "${TFLITE_BEAM_CORAL_LIBEDGETPU_URL}" -o "${LIBEDGETPU_ARCHIVE}"
    elif [ -e "$(which wget)" ]; then
      wget "${TFLITE_BEAM_CORAL_LIBEDGETPU_URL}" -O "${LIBEDGETPU_ARCHIVE}"
    else
      echo "cannot find curl or wget, cannot download libedgetpu runtime"
      exit 1
    fi
  fi
}

unarchive_libedgetpu() {
  if [ ! -d "${LIBEDGETPU_UNARCHIVED_DIR}" ]; then
    echo "Unarchiving libedgetpu..."
    rm -rf "${LIBEDGETPU_UNARCHIVED_DIR}" &&
    mkdir -p "${LIBEDGETPU_UNARCHIVED_DIR}" &&
    tar -C "${LIBEDGETPU_UNARCHIVED_DIR}" -xf "${LIBEDGETPU_ARCHIVE}"
  fi
}

copy_if_not_exists() {
    src="$1"
    dst="$2"
    if [ ! -e "$dst" ]; then
      cp -a "${src}" "${dst}"
    fi
}

download_libedgetpu && unarchive_libedgetpu

mkdir -p "${LIBEDGETPU_RUNTIME_PRIV}"
LIBEDGETPU_TFLITE_BEAM_CORAL_USB_THROTTLE="throttled"
if [ "${TFLITE_BEAM_CORAL_USB_THROTTLE}" = "false" ]; then
  LIBEDGETPU_TFLITE_BEAM_CORAL_USB_THROTTLE="direct"
fi

case "${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}" in
  x86_64-linux-gnu|aarch64-linux-gnu|armv7l-linux-gnueabihf|riscv64-linux-*|armv6-linux-*)
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_BEAM_CORAL_USB_THROTTLE}/${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.so.1.0" "${LIBEDGETPU_RUNTIME_PRIV}/libedgetpu.so.1"
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu.h"
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu_c.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu_c.h"
    cd "${LIBEDGETPU_RUNTIME_PRIV}" && rm -f "libedgetpu.so" && ln -s "libedgetpu.so.1" "libedgetpu.so" && chmod 0755 "libedgetpu.so.1" "libedgetpu.so"
  ;;
  x86_64-apple-darwin|aarch64-apple-darwin)
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_BEAM_CORAL_USB_THROTTLE}/${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.1.0.dylib" "${LIBEDGETPU_RUNTIME_PRIV}/libedgetpu.1.0.dylib"
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu.h"
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu_c.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu_c.h"
    cd "${LIBEDGETPU_RUNTIME_PRIV}" && rm -f "libedgetpu.1.dylib" "libedgetpu.dylib" && ln -s "libedgetpu.1.0.dylib" "libedgetpu.1.dylib" && ln -s "libedgetpu.1.dylib" "libedgetpu.dylib" && chmod 0755 "libedgetpu.1.0.dylib" "libedgetpu.1.dylib" "libedgetpu.dylib"
  ;;
  *)
    echo "Unknown triplet: ${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}. Will try to copy requested runtime."
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu.h"
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu_c.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu_c.h"

    export LIBEDGETPU_SO_FILE="${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_BEAM_CORAL_USB_THROTTLE}/${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.so.1"
    export LIBEDGETPU_DYLIB_FILE="${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_BEAM_CORAL_USB_THROTTLE}/${TFLITE_BEAM_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.1.0.dylib"
    if [ -e "${LIBEDGETPU_SO_FILE}" ]; then
      copy_if_not_exists "${LIBEDGETPU_SO_FILE}" "${LIBEDGETPU_RUNTIME_PRIV}/libedgetpu.so.1" ;
      cd "${LIBEDGETPU_RUNTIME_PRIV}" && rm -f "libedgetpu.so" && ln -s "libedgetpu.so.1" "libedgetpu.so" && chmod 0755 "libedgetpu.so.1" "libedgetpu.so"
    else
      if [ -e "${LIBEDGETPU_DYLIB_FILE}" ]; then
          copy_if_not_exists "${LIBEDGETPU_DYLIB_FILE}" "${LIBEDGETPU_RUNTIME_PRIV}/libedgetpu.1.0.dylib"
          cd "${LIBEDGETPU_RUNTIME_PRIV}" && rm -f "libedgetpu.1.dylib" "libedgetpu.dylib" && ln -s "libedgetpu.1.0.dylib" "libedgetpu.1.dylib" && ln -s "libedgetpu.1.dylib" "libedgetpu.dylib" && chmod 0755 "libedgetpu.1.0.dylib" "libedgetpu.1.dylib" "libedgetpu.dylib"
      else
          echo "No .so file or .dylib file is found."
          exit 1
      fi
    fi
  ;;
esac
