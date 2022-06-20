#!/bin/bash

TFLITE_ROOT_SRC="$1"
DESTDIR="$2"
cd "${TFLITE_ROOT_SRC}"
mkdir -p "${DESTDIR}"

CROSSCOMPILE=""
if [ -n "${TARGET_ARCH}" ] && [ -n "${TARGET_OS}" ] && [ -n "${TARGET_ABI}" ]; then
  CROSSCOMPILE="--host=${TARGET_ARCH}-${TARGET_OS}-${TARGET_ABI}"
fi
git submodule update --init 3rd_party/libusb
cd 3rd_party/libusb
./autogen.sh
./configure CC="${CC}" "${CROSSCOMPILE}" --enable-shared --enable-udev=no --prefix=/
make DESTDIR="${DESTDIR}" install
