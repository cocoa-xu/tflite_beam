#
# CMake Toolchain file for cross-compiling for armv6 on Linux (with nerves toolchain).
#
# curl -fSL https://github.com/nerves-project/toolchains/releases/download/v14.2.0/nerves_toolchain_armv6_nerves_linux_gnueabihf-linux_x86_64-14.2.0-11156D8.tar.xz -o nerves_toolchain_armv6_nerves_linux_gnueabihf-linux_x86_64-14.2.0-11156D8.tar.xz
# tar -xf nerves_toolchain_armv6_nerves_linux_gnueabihf-linux_x86_64-14.2.0-11156D8.tar.xz
# sudo mv nerves_toolchain_armv6_nerves_linux_gnueabihf-linux_x86_64-14.2.0 /usr/local/bin/nerves_toolchain_armv6_nerves_linux_gnueabihf-linux_x86_64-14.2.0
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR armv6)

# Nerves ships this toolchain for x86_64 and aarch64 hosts alike, so the host
# name belongs in a variable rather than in the path. Hardcoding x86_64 meant an
# arm64 machine could not run this build at all, which is the machine most likely
# to be reproducing an armv6 CI failure without waiting for CI.
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
            "/usr/local/bin/nerves_toolchain_armv6_nerves_linux_gnueabihf-${NERVES_TOOLCHAIN_HOST}-14.2.0")
    endif()
endif()
if(NOT EXISTS "${NERVES_TOOLCHAIN_ROOT}/bin/armv6-nerves-linux-gnueabihf-gcc")
    message(FATAL_ERROR
        "no armv6 Nerves toolchain at ${NERVES_TOOLCHAIN_ROOT}. Set the "
        "NERVES_TOOLCHAIN_ROOT environment variable, or NERVES_TOOLCHAIN_HOST "
        "if only the host part is wrong.")
endif()
set(CMAKE_C_COMPILER "${NERVES_TOOLCHAIN_ROOT}/bin/armv6-nerves-linux-gnueabihf-gcc")
set(CMAKE_CXX_COMPILER "${NERVES_TOOLCHAIN_ROOT}/bin/armv6-nerves-linux-gnueabihf-g++")

set(CMAKE_FIND_ROOT_PATH /usr/arm-linux-gnueabihf)

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
