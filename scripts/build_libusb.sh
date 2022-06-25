#!/bin/bash

TFLITE_ROOT_SRC="$1"
DESTDIR="$2"
cd "${TFLITE_ROOT_SRC}"
mkdir -p "${DESTDIR}"

git submodule update --init 3rd_party/libusb
cd 3rd_party/libusb
make clean
CROSSCOMPILE=""
if [ -n "${TARGET_ARCH}" ] && [ -n "${TARGET_OS}" ] && [ -n "${TARGET_ABI}" ]; then
  export CROSSCOMPILE="--host=${TARGET_ARCH}-${TARGET_OS}-${TARGET_ABI}"
  case "${TARGET_OS}" in
    apple*)
      ./autogen.sh CC="${CC}" CFLAGS="-arch ${TARGET_ARCH}" LDFLAGS="-arch ${TARGET_ARCH}" "${CROSSCOMPILE}" --enable-shared --disable-udev --prefix=/
      ./configure CC="${CC}" CFLAGS="-arch ${TARGET_ARCH}" LDFLAGS="-arch ${TARGET_ARCH}" "${CROSSCOMPILE}" --enable-shared --disable-udev --prefix=/
    ;;
    linux*)
      ./autogen.sh CC="${CC}" "${CROSSCOMPILE}" --enable-shared --disable-udev --prefix=/
      ./configure CC="${CC}" "${CROSSCOMPILE}" --enable-shared --disable-udev --prefix=/
    ;;
    *)
      echo "unsupported system: ${TARGET_OS}" ;
      exit 1 ;
    ;;
  esac
else
  ./autogen.sh CC="${CC}" --enable-shared --disable-udev --prefix=/
  ./configure CC="${CC}" --enable-shared --disable-udev --prefix=/
fi

make DESTDIR="${DESTDIR}" install

LIBUSB_SO="libusb-1.0.0.dylib"
LIBUSB_SO_SYMLINK="libusb-1.0.0.dylib"
if [ -n "${CROSSCOMPILE}" ] && [ "${TARGET_OS}" != "apple" ]; then
  export LIBUSB_SO="libusb-1.0.so.0.3.0"
  export LIBUSB_SO_SYMLINK="libusb-1.0.so.0"
fi
case "$(uname -s)" in
  Linux*)
    export LIBUSB_SO="libusb-1.0.so.0.3.0"
    export LIBUSB_SO_SYMLINK="libusb-1.0.so.0"
  ;;
esac
cd "${DESTDIR}"
rm -f "${LIBUSB_SO_SYMLINK}"
ln -s "lib/${LIBUSB_SO}" "${LIBUSB_SO_SYMLINK}"
