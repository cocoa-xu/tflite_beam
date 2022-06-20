#!/bin/bash

NATIVE_BINDINGS_SO="$1"
case "$(uname -s)" in
  Linux*)
    LIBEDGETPU_DIR="$(dirname "${NATIVE_BINDINGS_SO}")/libedgetpu"
    cd "${LIBEDGETPU_DIR}"
    patchelf --replace-needed libusb-1.0.so.0 \$ORIGIN/lib/libusb-1.0.so.0 libedgetpu.so.1.0
    # patchelf --replace-needed libedgetpu.so.1.0 \$ORIGIN/libedgetpu/libedgetpu.so.1.0 "${NATIVE_BINDINGS_SO}"
    ;;
  Darwin*)
    #    LIBEDGETPU_PATH="$(otool -L "${NATIVE_BINDINGS_SO}" | grep 'libedgetpu.1.dylib' | awk -F' \\(compatibility' '/,/{gsub(/[ \t]/, "", $1);print $1}')"
    #    EXPECTED_PATH="@loader_path/libedgetpu/libedgetpu.1.dylib"
    #    if [ -n "${LIBEDGETPU_PATH}" ]; then
    #      if [ "${LIBEDGETPU_PATH}" != "${EXPECTED_PATH}" ]; then
    #        install_name_tool -change "${LIBEDGETPU_PATH}" "${EXPECTED_PATH}" "${NATIVE_BINDINGS_SO}"
    #      fi
    #    fi

    LIBEDGETPU_DIR="$(dirname "${NATIVE_BINDINGS_SO}")/libedgetpu"
    cd "${LIBEDGETPU_DIR}"
    LIBEDGETPU_WRONG_NAME="$(find . -name '*edgetpu*' -size +100k -type f | head -n1)"
    if [ -z "${LIBEDGETPU_WRONG_NAME}" ]; then
      echo "cannot find libedgetpu.1.0.dylib in ${LIBEDGETPU_DIR}"
      exit 1
    fi

    mv "${LIBEDGETPU_WRONG_NAME}" "libedgetpu.1.0.dylib"
    rm -f "libedgetpu.1.dylib"
    ln -s "libedgetpu.1.0.dylib" "libedgetpu.1.dylib"
    ;;
  *)
    ;;
esac
