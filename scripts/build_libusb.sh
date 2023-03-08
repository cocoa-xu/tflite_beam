#!/bin/bash

LIBUSB_SRC="$1"
DESTDIR="$2"
PRIV_DIR="$3"
cd "${LIBUSB_SRC}"
mkdir -p "${DESTDIR}"

make clean
CROSSCOMPILE=""
if [ -n "${TARGET_ARCH}" ] && [ -n "${TARGET_OS}" ] && [ -n "${TARGET_ABI}" ]; then
  export CROSSCOMPILE="--host=${TARGET_ARCH}-${TARGET_OS}-${TARGET_ABI}"
  case "${TARGET_OS}" in
    apple*)
      ./configure CC="${CC}" CFLAGS="-arch ${TARGET_ARCH}" LDFLAGS="-arch ${TARGET_ARCH}" "${CROSSCOMPILE}" --enable-shared --disable-static --disable-udev --prefix=/
    ;;
    linux*)
      ./configure CC="${CC}" "${CROSSCOMPILE}" --enable-shared --disable-static --disable-udev --prefix=/
    ;;
    *)
      echo "unsupported system: ${TARGET_OS}" ;
      exit 1 ;
    ;;
  esac
else
  ./configure CC="${CC}" --enable-shared --disable-static --disable-udev --prefix=/
fi

make DESTDIR="${DESTDIR}" install

LIBUSB_SO="libusb-1.0.0.dylib"
if [ -n "${CROSSCOMPILE}" ] && [ "${TARGET_OS}" != "apple" ]; then
  export LIBUSB_SO="libusb-1.0.so.0.3.0"
fi
case "$(uname -s)" in
  Linux*)
    export LIBUSB_SO="libusb-1.0.so.0.3.0"
  ;;
esac

mkdir -p "${PRIV_DIR}/libedgetpu"
cp "${DESTDIR}/lib/${LIBUSB_SO}" "${PRIV_DIR}/libedgetpu"
