#
# CMake Toolchain file for cross-compiling for riscv64 on Linux (with nerves toolchain).
#
# curl -fSL https://github.com/nerves-project/toolchains/releases/download/v14.2.0/nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0-D43CCBE.tar.xz -o nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0-D43CCBE.tar.xz
# tar -xf nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0-D43CCBE.tar.xz
# sudo mv nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0 /usr/local/bin/nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR riscv64)

# See the note in armv6-nerves-linux-gnueabihf.cmake: the host part of the
# toolchain name is not always x86_64.
if(NOT NERVES_TOOLCHAIN_HOST)
    if(CMAKE_HOST_SYSTEM_PROCESSOR MATCHES "^(aarch64|arm64)$")
        set(NERVES_TOOLCHAIN_HOST "linux_aarch64")
    else()
        set(NERVES_TOOLCHAIN_HOST "linux_x86_64")
    endif()
endif()
if(NOT NERVES_TOOLCHAIN_ROOT)
    set(NERVES_TOOLCHAIN_ROOT
        "/usr/local/bin/nerves_toolchain_riscv64_nerves_linux_gnu-${NERVES_TOOLCHAIN_HOST}-14.2.0")
endif()
if(NOT EXISTS "${NERVES_TOOLCHAIN_ROOT}/bin/riscv64-nerves-linux-gnu-gcc")
    message(FATAL_ERROR
        "no riscv64 Nerves toolchain at ${NERVES_TOOLCHAIN_ROOT}. Set "
        "NERVES_TOOLCHAIN_ROOT, or NERVES_TOOLCHAIN_HOST if only the host part "
        "is wrong.")
endif()
set(CMAKE_C_COMPILER "${NERVES_TOOLCHAIN_ROOT}/bin/riscv64-nerves-linux-gnu-gcc")
set(CMAKE_CXX_COMPILER "${NERVES_TOOLCHAIN_ROOT}/bin/riscv64-nerves-linux-gnu-g++")

set(CMAKE_FIND_ROOT_PATH /usr/riscv64-linux-gnu)

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
