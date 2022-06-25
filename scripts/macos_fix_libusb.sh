#!/bin/bash

case "$(uname -s)" in
  Darwin*)
    LIBEDGETPU_FILE="$1"
    LIBUSB_IN_LIBEDGETPU="$(otool -L "${LIBEDGETPU_FILE}" | grep libusb | awk -F' \\(compatibility' '/,/{gsub(/[ \t]/, "", $1);print $1}')"
    LIBUSB="@loader_path/libusb-1.0.0.dylib"
    if [ "${LIBUSB_IN_LIBEDGETPU}" != "${LIBUSB}" ]; then
      install_name_tool -change "${LIBUSB_IN_LIBEDGETPU}" "${LIBUSB}" "${LIBEDGETPU_FILE}"
    fi

    LIBEDGETPU_DIR="$(dirname "${LIBEDGETPU_FILE}")"
    cd "${LIBEDGETPU_DIR}"
    if [ ! -L "libusb-1.0.0.dylib" ]; then
      rm -f "libusb-1.0.0.dylib"
      ln -s "lib/libusb-1.0.0.dylib" "libusb-1.0.0.dylib"
    fi
    ;;
  *)
    ;;
esac
