#!/bin/bash

case "$(uname -s)" in
  Linux*)
    LIBEDGETPU_DIR="$1"
    LIBEDGETPU_WRONG_NAME="$(find "${LIBEDGETPU_DIR}" -size +1M -type f | head -n1)"
    if [ -z "${LIBEDGETPU_WRONG_NAME}" ]; then
      echo "cannot find libedgetpu.so.1 or libedgetpu.so in ${LIBEDGETPU_DIR}"
      exit 1
    fi

    if [ ! -e "${LIBEDGETPU_DIR}/libedgetpu.so.1" ]; then
      cp -a "${LIBEDGETPU_WRONG_NAME}" "${LIBEDGETPU_DIR}/libedgetpu.so.1" ;
    fi
    if [ ! -e "${LIBEDGETPU_DIR}/libedgetpu.so" ]; then
      cd "${LIBEDGETPU_DIR}" ;
      ln -s "${LIBEDGETPU_WRONG_NAME}" "${LIBEDGETPU_DIR}/libedgetpu.so" ;
    fi
    ;;
  *)
    ;;
esac
