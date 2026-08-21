cmake_minimum_required(VERSION 3.16 FATAL_ERROR)
project(tflite_beam C CXX)

if(NOT DEFINED PRIV_DIR)
    if(DEFINED MIX_APP_PATH AND NOT "${MIX_APP_PATH}" STREQUAL "")
        set(PRIV_DIR "${MIX_APP_PATH}/priv")
    else()
        set(PRIV_DIR "${CMAKE_CURRENT_SOURCE_DIR}/priv")
    endif()
endif()
message(STATUS "Using PRIV_DIR: ${PRIV_DIR}")

if(DEFINED ERTS_INCLUDE_DIR AND NOT "${ERTS_INCLUDE_DIR}" STREQUAL "")
    set(ERTS_INCLUDE_DIR "${ERTS_INCLUDE_DIR}")
else()
    set(ERTS_INCLUDE_DIR_ONE_LINER "erl -noshell -eval \"io:format('~ts/erts-~ts/include/', [code:root_dir(), erlang:system_info(version)]), halt().\"")
    if(WIN32)
        execute_process(COMMAND powershell -command "${ERTS_INCLUDE_DIR_ONE_LINER}" OUTPUT_VARIABLE ERTS_INCLUDE_DIR)
    else()
        execute_process(COMMAND bash -c "${ERTS_INCLUDE_DIR_ONE_LINER}" OUTPUT_VARIABLE ERTS_INCLUDE_DIR)
    endif()
    set(ERTS_INCLUDE_DIR "${ERTS_INCLUDE_DIR}")
endif()
message(STATUS "Using ERTS_INCLUDE_DIR: ${ERTS_INCLUDE_DIR}")

set(TFLITE_ROOT_DIR "" CACHE PATH
    "Directory that contains the TensorFlow Lite project"
)
if(NOT TFLITE_ROOT_DIR)
    message(FATAL_ERROR "TFLITE_ROOT_DIR is not set.")
endif()

if(UNIX AND APPLE)
    set(CMAKE_SHARED_LINKER_FLAGS "-flat_namespace -undefined suppress -undefined dynamic_lookup")
endif()

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wno-comment")
if(DEFINED ENV{TARGET_GCC_FLAGS})
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} $ENV{TARGET_GCC_FLAGS}")
    if(DEFINED ENV{TARGET_CPU})
        if("$ENV{TARGET_CPU}" STREQUAL "cortex_a53" OR "$ENV{TARGET_CPU}" STREQUAL "cortex_a57")
            string(REPLACE "neon-vfpv4" "neon-fp-armv8" TARGET_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
            set(CMAKE_CXX_FLAGS "${TARGET_CXX_FLAGS}")
        endif()
    endif()
endif()
if(DEFINED ENV{TARGET_ARCH})
    if(NOT "$ENV{TARGET_ARCH}" STREQUAL "arm")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DTF_HAS_vcvtaq_s32_f32")
    endif()
endif()

message(STATUS "CMAKE_TOOLCHAIN_FILE: ${CMAKE_TOOLCHAIN_FILE}")
message(STATUS "CMAKE_CXX_FLAGS: ${CMAKE_CXX_FLAGS}")
message(STATUS "CMAKE_CXX_COMPILER_ID: ${CMAKE_CXX_COMPILER_ID}")
message(STATUS "CMAKE_CXX_COMPILER_VERSION: ${CMAKE_CXX_COMPILER_VERSION}")

set(CMAKE_MODULE_PATH
  "${TFLITE_ROOT_DIR}/tools/cmake/modules"
  ${CMAKE_MODULE_PATH}
)
set(ABSL_PROPAGATE_CXX_STD ON)
find_package(absl REQUIRED)

if(DEFINED ENV{TARGET_ARCH})
    if("$ENV{TARGET_ARCH}" MATCHES "armv7.*")
        set(CMAKE_SYSTEM_PROCESSOR "armv7l" CACHE STRING "Updates CMAKE_SYSTEM_PROCESSOR")
        set(TFLITE_ENABLE_XNNPACK OFF CACHE BOOL "Disable XNNPACK in TensorFlow Lite for armv7l")
    elseif("$ENV{TARGET_ARCH}" MATCHES "arm")
        if(DEFINED ENV{TARGET_CPU})
            if("$ENV{TARGET_CPU}" STREQUAL "cortex_a35" OR "$ENV{TARGET_CPU}" STREQUAL "cortex_a53" OR "$ENV{TARGET_CPU}" STREQUAL "cortex_a57" OR "$ENV{TARGET_CPU}" STREQUAL "cortex_a72" OR "$ENV{TARGET_CPU}" STREQUAL "cortex_a73")
                set(CMAKE_SYSTEM_PROCESSOR "armv8l" CACHE STRING "Updates CMAKE_SYSTEM_PROCESSOR")
            elseif("$ENV{TARGET_CPU}" STREQUAL "cortex_a8" OR "$ENV{TARGET_CPU}" STREQUAL "cortex_a7")
                set(CMAKE_SYSTEM_PROCESSOR "armv7l" CACHE STRING "Updates CMAKE_SYSTEM_PROCESSOR")
                set(TFLITE_ENABLE_XNNPACK OFF CACHE BOOL "Disable XNNPACK in TensorFlow Lite for armv7l")
            elseif("$ENV{TARGET_CPU}" MATCHES "arm1176.*" OR "$ENV{TARGET_CPU}" MATCHES "arm1156.*" OR "$ENV{TARGET_CPU}" MATCHES "arm1136.*")
                set(CMAKE_SYSTEM_PROCESSOR "armv6" CACHE STRING "Updates CMAKE_SYSTEM_PROCESSOR")
                set(TFLITE_ENABLE_XNNPACK OFF CACHE BOOL "Disable XNNPACK in TensorFlow Lite for armv6")
            endif()
        else()
            # assume armv6?
            set(CMAKE_SYSTEM_PROCESSOR "armv6" CACHE STRING "Updates CMAKE_SYSTEM_PROCESSOR")
            set(TFLITE_ENABLE_XNNPACK OFF CACHE BOOL "Disable XNNPACK in TensorFlow Lite for armv6")
        endif()
    endif()
endif()

if(DEFINED ENV{TARGET_ARCH})
    if(NOT "$ENV{TARGET_ARCH}" STREQUAL "arm")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DTF_HAS_vcvtaq_s32_f32")
    endif()
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    if (CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 10.0)
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-gnu-inline-cpp-without-extern")
    endif()
endif()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}" CACHE STRING "Override CXX FLAGS")

# reset CLOG_SOURCE_DIR so that cpuinfo won't complain
unset(CLOG_SOURCE_DIR CACHE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "NO shared library")
add_subdirectory(
    "${TFLITE_ROOT_DIR}"
    EXCLUDE_FROM_ALL
)

if(${TFLITE_BEAM_CORAL_SUPPORT})
    if(NOT GFLAGS_ROOT_DIR)
        message(FATAL_ERROR "GFLAGS_ROOT_DIR is not set.")
    endif()
    set(GFLAGS_ROOT_DIR "${GFLAGS_ROOT_DIR}" CACHE PATH
        "Directory that contains the gflags project"
    )

    add_subdirectory(
        "${GFLAGS_ROOT_DIR}"
        EXCLUDE_FROM_ALL
    )

    if(NOT GLOG_ROOT_DIR)
        message(FATAL_ERROR "GLOG_ROOT_DIR is not set.")
    endif()
    set(GLOG_ROOT_DIR "${GLOG_ROOT_DIR}" CACHE PATH
        "Directory that contains the glog project"
    )

    # glog looks up gflags with find_package, which can only ever resolve to a
    # system-wide copy -- never the one added above -- and the two sets of targets
    # then collide. Build hosts have no system gflags, so this is also the
    # configuration every released binary is already built with.
    set(WITH_GFLAGS OFF CACHE BOOL "Use gflags" FORCE)

    add_subdirectory(
        "${GLOG_ROOT_DIR}"
        EXCLUDE_FROM_ALL
    )
endif()

set(SOURCE_FILES
    "${C_SRC}/helper.cpp"
    "${C_SRC}/nif_utils.cpp"
    "${C_SRC}/fault_inject.cpp"
    "${C_SRC}/tflite/delegate.cpp"
    "${C_SRC}/tflite/error_reporter.cpp"
    "${C_SRC}/tflite/flatbuffer_model.cpp"
    "${C_SRC}/tflite/interpreter.cpp"
    "${C_SRC}/tflite/metadata.cpp"
    "${C_SRC}/tflite/interpreter_builder.cpp"
    "${C_SRC}/tflite/ops/builtin/builtin_resolver.cpp"
    "${C_SRC}/tflite/signature_runner.cpp"
    "${C_SRC}/tflite/status.cpp"
    "${C_SRC}/tflite/tflitetensor.cpp"
    "${C_SRC}/tflite/tflite.cpp"
    "${C_SRC}/bindings.cpp"
    "${C_SRC}/erlang_nif_resource.cpp"
)

if(${TFLITE_BEAM_CORAL_SUPPORT})
    include_directories("${C_SRC}/libcoral")
    include_directories("${C_SRC}/libcoral/libedgetpu")
    include_directories("${LIBUSB_INSTALL_DIR}/include")
    link_directories("${LIBUSB_INSTALL_DIR}/lib")

    set(CORAL_CLASSIFICATION_SOURCES "${C_SRC}/libcoral/coral/classification/adapter.cc")
    set(CORAL_DETECTION_SOURCES "${C_SRC}/libcoral/coral/detection/adapter.cc")
#    set(CORAL_LEARN_SOURCES
#        "${C_SRC}/libcoral/coral/learn/imprinting_engine.cc"
#        "${C_SRC}/libcoral/coral/learn/utils.cc"
#        "${C_SRC}/libcoral/coral/learn/backprop/layers.cc"
#        "${C_SRC}/libcoral/coral/learn/backprop/multi_variate_normal_distribution.cc"
#        "${C_SRC}/libcoral/coral/learn/backprop/softmax_regression_model.cc"
#    )
#    set(CORAL_PIPELINE_SOURCES
#        "${C_SRC}/libcoral/coral/pipeline/pipelined_model_runner.cc"
#        "${C_SRC}/libcoral/coral/pipeline/internal/memory_pool_allocator.cc"
#        "${C_SRC}/libcoral/coral/pipeline/internal/segment_runner.cc"
#    )
    set(CORAL_POSE_ESTIMATION_SOURCES
        "${C_SRC}/libcoral/coral/pose_estimation/posenet_decoder.cc"
        "${C_SRC}/libcoral/coral/pose_estimation/posenet_decoder_op.cc"
        "${C_SRC}/libcoral/coral/pose_estimation/posenet_decoder_tflite_plugin.cc"
    )
    set(CORAL_COMMON_SOURCES
        "${C_SRC}/libcoral/coral/error_reporter.cc"
        "${C_SRC}/libcoral/coral/tflite_utils.cc"
    )

    list(APPEND SOURCE_FILES "${CORAL_CLASSIFICATION_SOURCES}")
    list(APPEND SOURCE_FILES "${CORAL_DETECTION_SOURCES}")
    list(APPEND SOURCE_FILES "${CORAL_LEARN_SOURCES}")
    list(APPEND SOURCE_FILES "${CORAL_PIPELINE_SOURCES}")
    list(APPEND SOURCE_FILES "${CORAL_POSE_ESTIMATION_SOURCES}")
    list(APPEND SOURCE_FILES "${CORAL_COMMON_SOURCES}")

    list(APPEND SOURCE_FILES "${C_SRC}/coral/coral.cpp")
endif()

add_library(tflite_beam SHARED
    ${SOURCE_FILES}
)
install(
    TARGETS tflite_beam
    RUNTIME DESTINATION "${PRIV_DIR}"
)
set(TFLITE_BEAM_LINK_LIBRARIES "tensorflow-lite")
list(APPEND TFLITE_BEAM_LINK_LIBRARIES
    absl::any
    absl::flat_hash_map
    absl::flags
    absl::hash
    absl::status
    absl::strings
    absl::synchronization
    absl::variant
)
if(${TFLITE_BEAM_CORAL_SUPPORT})
    if(UNIX AND NOT APPLE)
        list(APPEND TFLITE_BEAM_LINK_LIBRARIES "edgetpu")
        list(APPEND TFLITE_BEAM_LINK_LIBRARIES "usb-1.0.0")
        target_link_directories(tflite_beam PUBLIC "${PRIV_DIR}/libedgetpu")
    elseif(UNIX AND APPLE)
        list(APPEND TFLITE_BEAM_LINK_LIBRARIES "edgetpu.1.0")
        list(APPEND TFLITE_BEAM_LINK_LIBRARIES "usb-1.0.0")
    endif()
    list(APPEND TFLITE_BEAM_LINK_LIBRARIES "gflags")
    list(APPEND TFLITE_BEAM_LINK_LIBRARIES "glog")
endif()

target_link_libraries(tflite_beam
    ${TFLITE_BEAM_LINK_LIBRARIES}
    ${CMAKE_DL_LIBS}
)
# A delegate plugin for the external-delegate tests to load. Off by default and
# never set by any released build; the output goes straight to test/plugin/,
# which is gitignored.
option(TFLITE_BEAM_BUILD_TEST_PLUGIN "Build the external-delegate test plugin" OFF)
if(TFLITE_BEAM_BUILD_TEST_PLUGIN)
    add_library(tflite_beam_test_delegate SHARED
        "${CMAKE_CURRENT_SOURCE_DIR}/test/plugin/tflite_beam_test_delegate.c")
    target_include_directories(tflite_beam_test_delegate PRIVATE "${TENSORFLOW_SOURCE_DIR}")
    set_target_properties(tflite_beam_test_delegate PROPERTIES
        PREFIX ""
        SUFFIX ".so"
        LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/test/plugin")
endif()

set_target_properties(tflite_beam PROPERTIES PREFIX "")
set_target_properties(tflite_beam PROPERTIES SUFFIX ".so")
set_target_properties(tflite_beam PROPERTIES
    INSTALL_RPATH_USE_LINK_PATH TRUE
    BUILD_WITH_INSTALL_RPATH TRUE
)

if(UNIX AND NOT APPLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-but-set-variable")
    set_target_properties(tflite_beam PROPERTIES INSTALL_RPATH "\$ORIGIN/libedgetpu")
elseif(UNIX AND APPLE)
    set(CMAKE_SHARED_LINKER_FLAGS "-flat_namespace -undefined suppress -undefined dynamic_lookup")
    set_target_properties(tflite_beam PROPERTIES INSTALL_RPATH "@loader_path/libedgetpu")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -I${ERTS_INCLUDE_DIR}")
if(${TFLITE_BEAM_CORAL_SUPPORT})
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -L${PRIV_DIR}/libedgetpu")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DCORAL_SUPPORT_ENABLED=1")
endif()
# The resolved cache value, read after TFLite has been added: the armv6/armv7l
# branches above turn XNNPACK off before that, and TFLite's own
# TFLITE_BUILD_WITH_XNNPACK_DELEGATE does not reach us through the
# plain-signature target_link_libraries.
# TfLite's own TfLiteVersion() cannot serve this purpose: lite/version.h carries
# a hand-maintained TF_MINOR_VERSION that upstream forgets to bump -- 2.21.0's
# tree still says 2.19 -- and it only applies when the build system does not
# inject one, which Bazel does and CMake does not. A delegate plugin has to match
# the source version, so record that.
if(TFLITE_BEAM_TFLITE_VERSION)
    add_compile_definitions(TFLITE_BEAM_TFLITE_VERSION="${TFLITE_BEAM_TFLITE_VERSION}")
endif()
if(TFLITE_ENABLE_XNNPACK)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DTFLITE_BEAM_XNNPACK_ENABLED=1")
endif()
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -g3")
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
endif()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-unused-function -Wno-sign-compare -Wno-unused-parameter -Wno-missing-field-initializers -Wno-deprecated-declarations")
