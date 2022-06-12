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
set(SOURCE_FILES "${C_SRC}/bindings.cpp")
if(${TFLITE_ELIXIR_CORAL_SUPPORT})
    include_directories("${TFLITE_ELIXIR_CACHE_DIR}/${TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME}/edgetpu_runtime/libedgetpu")
    file(GLOB_RECURSE CORAL_SUPPORT_SOURCES "${C_SRC}/coral/*.cpp")
    list(APPEND SOURCE_FILES "${CORAL_SUPPORT_SOURCES}")
endif()
message(${SOURCE_FILES})
add_library(tflite_elixir SHARED
    ${SOURCE_FILES}
)
if(${TFLITE_ELIXIR_CORAL_SUPPORT})
    target_link_libraries(tflite_elixir
        tensorflow-lite
        edgetpu.1.0
    )
else()
    target_link_libraries(tflite_elixir
        tensorflow-lite
    )
endif()
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

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -I${ERTS_INCLUDE_DIR}")
if(${TFLITE_ELIXIR_CORAL_SUPPORT})
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -L${PRIV_DIR}/libedgetpu")
endif()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -Wall -Wextra -Wno-unused-parameter -Wno-missing-field-initializers -Wno-deprecated-declarations")
