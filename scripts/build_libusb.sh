#!/bin/bash

LIBUSB_SOURCE_URL="$1"
LIBUSB_SOURCE_ARCHIVE="$2"
THIRD_PARTY_DIR="$3"
LIBUSB_SRC="$4"
DESTDIR="$5"
PRIV_DIR="$6"

download_libusb() {
  if [ ! -e "${LIBUSB_SOURCE_ARCHIVE}" ]; then
    echo "Downloading libusb..."
    if [ -e "$(which curl)" ]; then
      curl -fSL "${LIBUSB_SOURCE_URL}" -o "${LIBUSB_SOURCE_ARCHIVE}"
    elif [ -e "$(which wget)" ]; then
      wget "${LIBUSB_SOURCE_URL}" -O "${LIBUSB_SOURCE_ARCHIVE}"
    else
      echo "cannot find curl or wget, cannot download libusb source code"
      exit 1
    fi
  fi
}

unarchive_libusb() {
  if [ ! -d "${LIBUSB_SOURCE_DIR}" ]; then
    echo "Unarchiving libusb..."
    rm -rf "${LIBUSB_SOURCE_DIR}" &&
    tar -C "${THIRD_PARTY_DIR}" -xf "${LIBUSB_SOURCE_ARCHIVE}"
  fi
}

download_libusb && unarchive_libusb

cd "${LIBUSB_SRC}"
mkdir -p "${DESTDIR}"

make clean
echo "libusb: ${TARGET_ARCH}-${TARGET_OS}-${TARGET_ABI}"
if [ -n "${TARGET_ARCH}" ] && [ -n "${TARGET_OS}" ] && [ -n "${TARGET_ABI}" ]; then
  case "${TARGET_OS}" in
    apple*)
      case "${TARGET_ARCH}" in
        x86_64*)
          CC="gcc -arch x86_64" CFLAGS="-fPIC -arch x86_64" LDFLAGS="-arch x86_64" ./configure --host="x86_64-apple-darwin" --enable-shared --disable-static --disable-udev --prefix=/
        ;;
        aarch64*)
          CC="gcc -arch arm64" CFLAGS="-fPIC -arch arm64" LDFLAGS="-arch arm64" ./configure --host="aarch64-apple-darwin" --enable-shared --disable-static --disable-udev --prefix=/
        ;;
        *)
          CC="gcc -arch ${TARGET_ARCH}" CFLAGS="-fPIC -arch ${TARGET_ARCH}" LDFLAGS="-arch ${TARGET_ARCH}" ./configure --host="${TARGET_ARCH}-${TARGET_OS}-${TARGET_ABI}" --enable-shared --disable-static --disable-udev --prefix=/
        ;;
      esac
    ;;
    linux*)
      ./configure CFLAGS="-fPIC" --host="${TARGET_ARCH}-${TARGET_OS}-${TARGET_ABI}" --enable-shared --disable-static --disable-udev --prefix=/
    ;;
    *)
      echo "unsupported system: ${TARGET_OS}" ;
      exit 1 ;
    ;;
  esac
else
  ./configure --enable-shared --disable-static --disable-udev --prefix=/
fi

make DESTDIR="${DESTDIR}" install

LIBUSB_SO="libusb-1.0.0.dylib"
LIBUSB_SO_SYMLINK="libusb-1.0.dylib"
LIBUSB_SO_SYMLINK2="libusb-1.dylib"

if [ -n "${CROSSCOMPILE}" ] && [ "${TARGET_OS}" != "apple" ]; then
  export LIBUSB_SO="libusb-1.0.so.0.3.0"
  export LIBUSB_SO_SYMLINK="libusb-1.0.0.so"
  export LIBUSB_SO_SYMLINK2="libusb-1.0.so.0"
fi
case "$(uname -s)" in
  Linux*)
    export LIBUSB_SO="libusb-1.0.so.0.3.0"
    export LIBUSB_SO_SYMLINK="libusb-1.0.0.so"
    export LIBUSB_SO_SYMLINK2="libusb-1.0.so.0"
  ;;
esac

mkdir -p "${PRIV_DIR}/libedgetpu"
cp "${DESTDIR}/lib/${LIBUSB_SO}" "${PRIV_DIR}/libedgetpu"
cd "${PRIV_DIR}/libedgetpu"
rm -f "${LIBUSB_SO_SYMLINK}"
rm -f "${LIBUSB_SO_SYMLINK2}"
ln -s "${LIBUSB_SO}" "${LIBUSB_SO_SYMLINK}"
ln -s "${LIBUSB_SO}" "${LIBUSB_SO_SYMLINK2}"
