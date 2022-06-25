#!/bin/bash

NATIVE_BINDINGS_SO="$1"
case "$(uname -s)" in
  Linux*)
    LIBEDGETPU_DIR="$(dirname "${NATIVE_BINDINGS_SO}")/libedgetpu"
    cd "${LIBEDGETPU_DIR}"
    patchelf --replace-needed libusb-1.0.so.0 \$ORIGIN/lib/libusb-1.0.so.0 libedgetpu.so.1.0
    ;;
  *)
    ;;
esac
