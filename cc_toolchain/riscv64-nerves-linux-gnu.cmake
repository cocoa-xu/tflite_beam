#
# CMake Toolchain file for cross-compiling for riscv64 on Linux (with nerves toolchain).
#
# curl -fSL https://github.com/nerves-project/toolchains/releases/download/v13.2.0/nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-13.2.0-DB80D1B.tar.xz -o nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-13.2.0-DB80D1B.tar.xz
# tar -xf nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-13.2.0-DB80D1B.tar.xz
# sudo mv nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-13.2.0 /usr/local/bin/nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-13.2.0
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR armv6)

set(CMAKE_C_COMPILER "/usr/local/bin/nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-13.2.0/bin/riscv64-nerves-linux-gnueabihf-gcc")
set(CMAKE_CXX_COMPILER "/usr/local/bin/nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-13.2.0/bin/riscv64-nerves-linux-gnueabihf-g++")

set(CMAKE_FIND_ROOT_PATH /usr/arm-linux-gnueabihf)

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
