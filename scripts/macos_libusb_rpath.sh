#!/bin/bash

if [ "${TARGET_OS}" = "linux" ]; then
  exit 0
fi

case "$(uname -s)" in
  Darwin*)
    TFLITE_BEAM_SO_FILE="$1"
    LIBUSB_IN_TFLITE_BEAM_SO="$(otool -L "${TFLITE_BEAM_SO_FILE}" | grep libusb | awk -F' \\(compatibility' '/,/{gsub(/[ \t]/, "", $1);print $1}')"
    LIBUSB="@rpath/libusb-1.0.0.dylib"
    if [ "${LIBUSB_IN_TFLITE_BEAM_SO}" != "${LIBUSB}" ]; then
      install_name_tool -change "${LIBUSB_IN_TFLITE_BEAM_SO}" "${LIBUSB}" "${TFLITE_BEAM_SO_FILE}"
    fi
    ;;
  *)
    ;;
esac
