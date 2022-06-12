#!/bin/bash

case "$(uname -s)" in
  Linux*)
    LIBEDGETPU_DIR="$1"
    LIBEDGETPU_WRONG_NAME="$(find "${LIBEDGETPU_DIR}" -size +1M -type f)"
    if [ "${LIBEDGETPU_WRONG_NAME}" != "${LIBEDGETPU_DIR}/libedgetpu.so.1" ]; then
      if [ -z "${LIBEDGETPU_WRONG_NAME}" ]; then
        echo "cannot find libedgetpu in ${LIBEDGETPU_DIR}"
        exit 1
      fi
      cp -a "${LIBEDGETPU_WRONG_NAME}" "${LIBEDGETPU_DIR}/libedgetpu.so.1" ;
      cp -a "${LIBEDGETPU_WRONG_NAME}" "${LIBEDGETPU_DIR}/libedgetpu.so" ;
      find "${LIBEDGETPU_DIR}" -size -1024c -exec rm -f '{}' \;
    fi
    ;;
  *)
    ;;
esac
