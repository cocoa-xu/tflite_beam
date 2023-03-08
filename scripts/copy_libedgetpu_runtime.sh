#!/bin/bash

LIBEDGETPU_RUNTIME_PRIV="$1"
TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR="$2"
TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET="$3"
TFLITE_ELIXIR_CORAL_USB_THROTTLE="$4"

function copy_if_not_exists() {
    src="$1"
    dst="$2"
    if [ ! -e "$dst" ]; then
      cp -a "${src}" "${dst}"
    fi
}

mkdir -p "${LIBEDGETPU_RUNTIME_PRIV}"
LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE="throttled"
if [ "${TFLITE_ELIXIR_CORAL_USB_THROTTLE}" = "NO" ]; then
  LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE="direct"
fi

case "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}" in
  x86_64-linux-gnu|aarch64-linux-gnu|armv7a-linux-gnueabihf|riscv64-linux-gnu)
    copy_if_not_exists "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE}/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.so.1.0" "${LIBEDGETPU_RUNTIME_PRIV}/libedgetpu.so.1.0"
    copy_if_not_exists "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR}/edgetpu_runtime/libedgetpu/edgetpu.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu.h"
    copy_if_not_exists "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR}/edgetpu_runtime/libedgetpu/edgetpu_c.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu_c.h"
  ;;
  x86_64-apple-darwin|aarch64-apple-darwin)
    copy_if_not_exists "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE}/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.1.0.dylib" "${LIBEDGETPU_RUNTIME_PRIV}/libedgetpu.1.0.dylib"
    copy_if_not_exists "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR}/edgetpu_runtime/libedgetpu/edgetpu.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu.h"
    copy_if_not_exists "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR}/edgetpu_runtime/libedgetpu/edgetpu_c.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu_c.h"
  ;;
  *)
    echo "Unknown triplet: ${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}. Will try to copy requested runtime."
    copy_if_not_exists "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR}/edgetpu_runtime/libedgetpu/edgetpu.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu.h"
    copy_if_not_exists "${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR}/edgetpu_runtime/libedgetpu/edgetpu_c.h" "${LIBEDGETPU_RUNTIME_PRIV}/edgetpu_c.h"

    export LIBEDGETPU_SO_FILE="${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE}/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.so.1.0"
    export LIBEDGETPU_DYLIB_FILE="${TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR}/edgetpu_runtime/libedgetpu/${LIBEDGETPU_TFLITE_ELIXIR_CORAL_USB_THROTTLE}/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET}/libedgetpu.1.0.dylib"
    if [ -e "${LIBEDGETPU_SO_FILE}" ]; then
      copy_if_not_exists "${LIBEDGETPU_SO_FILE}" "${LIBEDGETPU_RUNTIME_PRIV}/libedgetpu.so.1.0" ;
    else
      if [ -e "${LIBEDGETPU_DYLIB_FILE}" ]; then
          copy_if_not_exists "${LIBEDGETPU_DYLIB_FILE}" "${LIBEDGETPU_RUNTIME_PRIV}/libedgetpu.1.0.dylib"
      else
          echo "No .so file or .dylib file is found."
          exit 1
      fi
    fi
  ;;
esac
