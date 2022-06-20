#!/bin/bash

case "$(uname -s)" in
  Linux*)
    LIBEDGETPU_DIR="$1"
    mkdir -p "${LIBEDGETPU_DIR}"
    cd "${LIBEDGETPU_DIR}"
    LIBEDGETPU_WRONG_NAME="$(find . -name '*edgetpu*' -size +100k -type f | head -n1)"
    if [ -z "${LIBEDGETPU_WRONG_NAME}" ]; then
      echo "cannot find libedgetpu.so.1.0 in ${LIBEDGETPU_DIR}"
      exit 1
    fi

    if [ "${LIBEDGETPU_WRONG_NAME}" != "libedgetpu.so.1.0" ]; then
      mv "${LIBEDGETPU_WRONG_NAME}" "libedgetpu.so.1.0"
    fi
    rm -f "${LIBEDGETPU_DIR}/libedgetpu.so.1"
    ln -s "libedgetpu.so.1.0" "libedgetpu.so.1"
    rm -f "${LIBEDGETPU_DIR}/libedgetpu.so"
    ln -s "libedgetpu.so.1" "libedgetpu.so"
    ;;
  *)
    ;;
esac
