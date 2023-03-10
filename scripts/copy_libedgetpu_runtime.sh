#!/bin/bash

LIBEDGETPU_RUNTIME_PRIV="$1"
TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNARCHIVED_DIR="$2"
TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET="$3"
TFLITE_ELIXIR_CORAL_USB_THROTTLE="$4"
TFLITE_ELIXIR_CORAL_LIBEDGETPU_URL="$5"
TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME="$6"
TFLITE_ELIXIR_CACHE_DIR="$7"

LIBEDGETPU_ARCHIVE="${TFLITE_ELIXIR_CACHE_DIR}/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME}.tar.gz"
LIBEDGETPU_UNARCHIVED_DIR="${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNARCHIVED_DIR}"

download_libedgetpu() {
  if [ ! -e "${LIBEDGETPU_ARCHIVE}" ]; then
    echo "Downloading libedgetpu..."
    if [ -e "$(which curl)" ]; then
      curl -fSL "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_URL}" -o "${LIBEDGETPU_ARCHIVE}"
    elif [ -e "$(which wget)" ]; then
      wget "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_URL}" -O "${LIBEDGETPU_ARCHIVE}"
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
LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE="throttled"
if [ "${TFLITE_ELIXIR_CORAL_USB_THROTTLE}" = "NO" ]; then
  LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE="direct"
fi

case "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}" in
  x86_64-linux-gnu|aarch64-linux-gnu|armv7l-linux-gnueabihf|riscv64-linux-gnu)
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE}/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.so.1.0" "${LIBEDGETPU_RUNTIME_PRIV}/libedgetpu.so.1"
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu.h"
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu_c.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu_c.h"
    cd "${LIBEDGETPU_RUNTIME_PRIV}" && rm -f "libedgetpu.so" && ln -s "libedgetpu.so.1" "libedgetpu.so" && chmod 0755 "libedgetpu.so.1" "libedgetpu.so"
  ;;
  x86_64-apple-darwin|aarch64-apple-darwin)
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE}/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.1.0.dylib" "${LIBEDGETPU_RUNTIME_PRIV}/libedgetpu.1.0.dylib"
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu.h"
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu_c.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu_c.h"
    cd "${LIBEDGETPU_RUNTIME_PRIV}" && rm -f "libedgetpu.1.dylib" "libedgetpu.dylib" && ln -s "libedgetpu.1.0.dylib" "libedgetpu.1.dylib" && ln -s "libedgetpu.1.dylib" "libedgetpu.dylib" && chmod 0755 "libedgetpu.1.0.dylib" "libedgetpu.1.dylib" "libedgetpu.dylib"
  ;;
  *)
    echo "Unknown triplet: ${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}. Will try to copy requested runtime."
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu.h"
    copy_if_not_exists "${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/edgetpu_c.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu_c.h"

    export LIBEDGETPU_SO_FILE="${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE}/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.so.1"
    export LIBEDGETPU_DYLIB_FILE="${LIBEDGETPU_UNARCHIVED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE}/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.1.0.dylib"
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
