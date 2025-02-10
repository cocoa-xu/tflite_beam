#
# CMake Toolchain file for cross-compiling for riscv64 on Linux (with nerves toolchain).
#
# curl -fSL https://github.com/nerves-project/toolchains/releases/download/v14.2.0/nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0-D43CCBE.tar.xz -o nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0-D43CCBE.tar.xz
# tar -xf nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0-D43CCBE.tar.xz
# sudo mv nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0 /usr/local/bin/nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR riscv64)

set(CMAKE_C_COMPILER "/usr/local/bin/nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0/bin/riscv64-nerves-linux-gnu-gcc")
set(CMAKE_CXX_COMPILER "/usr/local/bin/nerves_toolchain_riscv64_nerves_linux_gnu-linux_x86_64-14.2.0/bin/riscv64-nerves-linux-gnu-g++")

set(CMAKE_FIND_ROOT_PATH /usr/riscv64-linux-gnu)

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
