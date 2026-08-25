#
# CMake Toolchain file for cross-compiling for armv7l on Linux (ubuntu 20.04).
#
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR armv7l)

# The abseil LiteRT pins refuses anything below GCC 10, and Ubuntu 20.04's
# unsuffixed arm-linux-gnueabihf-gcc is 9. Installing gcc-10-arm-linux-gnueabihf
# gives the suffixed name only, so take whatever CC and CXX say when they are
# set and fall back to the unsuffixed pair otherwise. The environment reaches
# the compiler test and every FetchContent sub-build; a -D does not.
if(DEFINED ENV{CC})
    set(CMAKE_C_COMPILER "$ENV{CC}")
else()
    set(CMAKE_C_COMPILER "/usr/bin/arm-linux-gnueabihf-gcc")
endif()
if(DEFINED ENV{CXX})
    set(CMAKE_CXX_COMPILER "$ENV{CXX}")
else()
    set(CMAKE_CXX_COMPILER "/usr/bin/arm-linux-gnueabihf-g++")
endif()
if(NOT EXISTS "${CMAKE_C_COMPILER}")
    message(FATAL_ERROR "no armv7l compiler at ${CMAKE_C_COMPILER}. Ubuntu 20.04 "
                        "ships gcc-10-arm-linux-gnueabihf under the -10 suffix; "
                        "point CC and CXX at it.")
endif()

set(CMAKE_FIND_ROOT_PATH /usr/arm-linux-gnueabihf)

set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
