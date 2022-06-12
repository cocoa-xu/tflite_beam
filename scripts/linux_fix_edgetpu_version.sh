#!/bin/bash

case "$(uname -s)" in
  Linux*)
    LIBEDGETPU_DIR="$1"
    LIBEDGETPU_WRONG_NAME="$(find "${LIBEDGETPU_DIR}" -size 1M -type f)"
    if [ "${LIBEDGETPU_WRONG_NAME}" != "${LIBEDGETPU_DIR}/libedgetpu.so" ]; then
      mv "${LIBEDGETPU_WRONG_NAME}" "${LIBEDGETPU_DIR}/libedgetpu.so" ;
    fi
    ;;
  *)
    ;;
esac
