#
# CMake toolchain file for cross-compiling armv7l with the Nerves toolchain.
#
# NOT what the published armv7l artifact is built with, and it should not become
# that. This toolchain links against glibc 2.38, where the released armv7l has
# asked for no more than 2.29 since v0.3.12. Raspberry Pi OS Bookworm carries
# 2.36, so a build from here would refuse to load on the commonest armv7l target
# there is.
#
# It is here for building against a Nerves system, which ships a matching glibc.
# For the published artifact see armv7l-linux-gnueabihf.cmake, which stays on
# Ubuntu 20.04's sysroot and only needs gcc-10 to satisfy the abseil that LiteRT
# pins.
#
# Fetch it with:
# curl -fSL https://github.com/nerves-project/toolchains/releases/download/v14.2.0/nerves_toolchain_armv7_nerves_linux_gnueabihf-linux_aarch64-14.2.0-6442318.tar.xz -o tc.tar.xz
# tar -xf tc.tar.xz
# sudo mv nerves_toolchain_armv7_nerves_linux_gnueabihf-linux_aarch64-14.2.0 /usr/local/bin/
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR armv7l)

# See the note in armv6-nerves-linux-gnueabihf.cmake: the host part of the
# toolchain name is not always x86_64.
# Both the compiler test and every FetchContent dependency re-read this file
# from a CMake process of their own, and neither inherits a -D from the command
# line. An environment variable reaches all of them, so that is the override
# that actually works; the list below is what makes -D work for the compiler
# test as well.
list(APPEND CMAKE_TRY_COMPILE_PLATFORM_VARIABLES
     NERVES_TOOLCHAIN_ROOT NERVES_TOOLCHAIN_HOST)

if(NOT NERVES_TOOLCHAIN_HOST)
    if(DEFINED ENV{NERVES_TOOLCHAIN_HOST})
        set(NERVES_TOOLCHAIN_HOST "$ENV{NERVES_TOOLCHAIN_HOST}")
    elseif(CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "^(aarch64|arm64)$")
        set(NERVES_TOOLCHAIN_HOST "linux_aarch64")
    else()
        set(NERVES_TOOLCHAIN_HOST "linux_x86_64")
    endif()
endif()
if(NOT NERVES_TOOLCHAIN_ROOT)
    if(DEFINED ENV{NERVES_TOOLCHAIN_ROOT})
        set(NERVES_TOOLCHAIN_ROOT "$ENV{NERVES_TOOLCHAIN_ROOT}")
    else()
        set(NERVES_TOOLCHAIN_ROOT
            "/usr/local/bin/nerves_toolchain_armv7_nerves_linux_gnueabihf-${NERVES_TOOLCHAIN_HOST}-14.2.0")
    endif()
endif()
if(NOT EXISTS "${NERVES_TOOLCHAIN_ROOT}/bin/armv7-nerves-linux-gnueabihf-gcc")
    message(FATAL_ERROR
        "no armv7 Nerves toolchain at ${NERVES_TOOLCHAIN_ROOT}. Set the "
        "NERVES_TOOLCHAIN_ROOT environment variable, or NERVES_TOOLCHAIN_HOST "
        "if only the host part is wrong.")
endif()
set(CMAKE_C_COMPILER "${NERVES_TOOLCHAIN_ROOT}/bin/armv7-nerves-linux-gnueabihf-gcc")
set(CMAKE_CXX_COMPILER "${NERVES_TOOLCHAIN_ROOT}/bin/armv7-nerves-linux-gnueabihf-g++")

set(CMAKE_FIND_ROOT_PATH /usr/arm-linux-gnueabihf)

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
