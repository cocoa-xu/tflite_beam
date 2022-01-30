cmake_minimum_required(VERSION 3.16 FATAL_ERROR)
project(tflite_elixir C CXX)

set(TFLITE_ROOT_DIR "" CACHE PATH
    "Directory that contains the TensorFlow Lite project"
)
if(NOT TFLITE_ROOT_DIR)
    message(FATAL_ERROR "TFLITE_ROOT_DIR is not set.")
endif()

add_subdirectory(
    "${TFLITE_ROOT_DIR}"
    EXCLUDE_FROM_ALL
)

set(CMAKE_CXX_STANDARD 11)
add_library(tflite_elixir SHARED
    "${C_SRC}/bindings.cpp"
)
target_link_libraries(tflite_elixir
    tensorflow-lite
)
set_target_properties(tflite_elixir PROPERTIES PREFIX "")
set_target_properties(tflite_elixir PROPERTIES SUFFIX ".so")

if(UNIX AND NOT APPLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -shared")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-but-set-variable")
else()
    # Although the compiler complains about not using these,
    # things only work with them set
    set(CMAKE_SHARED_LINKER_FLAGS "-flat_namespace -undefined suppress -undefined dynamic_lookup")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -I${ERTS_INCLUDE_DIR} -O3 -Wall -Wextra -Wno-unused-parameter -Wno-missing-field-initializers -Wno-deprecated-declarations")
