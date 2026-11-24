cmake_minimum_required(VERSION 3.16 FATAL_ERROR)
project(tflite_beam C CXX)

set(TFLITE_BEAM_USE_CCACHE "auto" CACHE STRING
    "Use ccache as the CMake compiler launcher: auto, true/ON, or false/OFF"
)
string(TOLOWER "${TFLITE_BEAM_USE_CCACHE}" _tflite_beam_use_ccache)
if(_tflite_beam_use_ccache STREQUAL "auto"
   OR _tflite_beam_use_ccache STREQUAL "true"
   OR _tflite_beam_use_ccache STREQUAL "on"
   OR _tflite_beam_use_ccache STREQUAL "1"
   OR _tflite_beam_use_ccache STREQUAL "yes")
    # Search on the build host even when a cross-compilation toolchain file
    # restricts program lookup to the target sysroot.
    find_program(TFLITE_BEAM_CCACHE_PROGRAM ccache NO_CMAKE_FIND_ROOT_PATH)
    if(TFLITE_BEAM_CCACHE_PROGRAM)
        set(_tflite_beam_ccache_langs "")
        foreach(_lang IN ITEMS C CXX OBJC OBJCXX)
            if(NOT DEFINED CMAKE_${_lang}_COMPILER_LAUNCHER
               AND NOT DEFINED ENV{CMAKE_${_lang}_COMPILER_LAUNCHER})
                set(CMAKE_${_lang}_COMPILER_LAUNCHER "${TFLITE_BEAM_CCACHE_PROGRAM}")
                list(APPEND _tflite_beam_ccache_langs "${_lang}")
            endif()
        endforeach()
        if(_tflite_beam_ccache_langs)
            list(JOIN _tflite_beam_ccache_langs ", " _tflite_beam_ccache_langs_str)
            message(STATUS "Using ccache (${_tflite_beam_ccache_langs_str}): ${TFLITE_BEAM_CCACHE_PROGRAM}")
        else()
            message(STATUS "Compiler launchers already configured; skipping ccache auto-configuration")
        endif()
    elseif(NOT _tflite_beam_use_ccache STREQUAL "auto")
        message(FATAL_ERROR
            "TFLITE_BEAM_USE_CCACHE=${TFLITE_BEAM_USE_CCACHE} requested, "
            "but ccache was not found in PATH.")
    else()
        message(STATUS "ccache not found; building without compiler cache")
    endif()
elseif(_tflite_beam_use_ccache STREQUAL "false"
       OR _tflite_beam_use_ccache STREQUAL "off"
       OR _tflite_beam_use_ccache STREQUAL "0"
       OR _tflite_beam_use_ccache STREQUAL "no")
    message(STATUS "ccache disabled (TFLITE_BEAM_USE_CCACHE=${TFLITE_BEAM_USE_CCACHE})")
else()
    message(FATAL_ERROR
        "Invalid TFLITE_BEAM_USE_CCACHE value '${TFLITE_BEAM_USE_CCACHE}'; "
        "expected auto, true/ON, or false/OFF.")
endif()

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

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -Wno-comment")
if(DEFINED ENV{TARGET_GCC_FLAGS})
    # These come from the Nerves toolchain conventions, which are written for
    # building executables. This builds a shared object, and -fPIE says the
    # opposite of -fPIC: it lets the compiler emit the local-exec TLS model,
    # whose relocations a shared object is not allowed to contain. On armv6 that
    # is a link failure rather than a warning, and it surfaces inside abseil
    # rather than in anything here:
    #
    #     R_ARM_TLS_LE32 relocation not permitted in shared object
    #
    # It went unnoticed because the line below used to read ${TARGET_GCC_FLAGS},
    # a CMake variable nothing sets, while the guard above tested the environment
    # one. None of these flags reached the compiler at all until that was fixed,
    # and the first build that actually applied them did not link.
    separate_arguments(TARGET_GCC_FLAGS_LIST NATIVE_COMMAND "$ENV{TARGET_GCC_FLAGS}")
    list(REMOVE_ITEM TARGET_GCC_FLAGS_LIST -fPIE -fpie -pie)
    list(JOIN TARGET_GCC_FLAGS_LIST " " TARGET_GCC_FLAGS_STR)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${TARGET_GCC_FLAGS_STR}")
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

# The one flag this build cannot be right without. Everything above appends, so
# anything that arrives later can undo it, and the way that fails is a link error
# thousands of lines into a dependency rather than a word about position
# independence. Say it here instead.
if(NOT CMAKE_CXX_FLAGS MATCHES "(^| )-fPIC( |$)")
    message(FATAL_ERROR
        "-fPIC is missing from CMAKE_CXX_FLAGS, which cannot build a shared "
        "object: ${CMAKE_CXX_FLAGS}")
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

# LiteRT 2.4.0's eigen.cmake and ml_dtypes.cmake fetch pinned archives from
# storage.googleapis.com/mirror.tensorflow.org by default instead of cloning
# gitlab.com, so no GitLab mirror override is needed here anymore.

# reset CLOG_SOURCE_DIR so that cpuinfo won't complain
unset(CLOG_SOURCE_DIR CACHE)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "NO shared library")
# Name the binary directory rather than letting CMake derive one. LiteRT's tflite
# subtree lives outside this project, and add_subdirectory refuses an out-of-tree
# source without somewhere to put its build.
add_subdirectory(
    "${TFLITE_ROOT_DIR}"
    "${CMAKE_CURRENT_BINARY_DIR}/tflite_runtime"
    EXCLUDE_FROM_ALL
)

# KleidiAI is fetched by XNNPACK from gitlab.arm.com, which GitHub's runners
# cannot reach, so a mirrored copy is handed over instead. The commit has to be
# the one XNNPACK asks for: an older one is missing headers the newer
# microkernels include, and the build says so two thirds of the way in as a file
# that does not exist rather than as a version that does not match. Say it here.
if(KLEIDIAI_SOURCE_DIR)
    set(_kleidiai_recipe "${CMAKE_BINARY_DIR}/xnnpack/cmake/DownloadKleidiAI.cmake")
    if(EXISTS "${_kleidiai_recipe}")
        file(STRINGS "${_kleidiai_recipe}" _kleidiai_url REGEX "kleidiai-[0-9a-f]+")
        string(REGEX MATCH "[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]+" _kleidiai_wanted "${_kleidiai_url}")
        get_filename_component(_kleidiai_have "${KLEIDIAI_SOURCE_DIR}" NAME)
        string(REGEX MATCH "[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]+" _kleidiai_have "${_kleidiai_have}")
        if(_kleidiai_wanted AND _kleidiai_have AND NOT _kleidiai_wanted STREQUAL _kleidiai_have)
            message(FATAL_ERROR
                "the mirrored KleidiAI is ${_kleidiai_have} and this XNNPACK "
                "wants ${_kleidiai_wanted}. Set KLEIDIAI_COMMIT in the Makefile "
                "to the latter.")
        endif()
    endif()
endif()

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
    "${C_SRC}/elitte/delegate.cpp"
    "${C_SRC}/elitte/error_reporter.cpp"
    "${C_SRC}/elitte/flatbuffer_model.cpp"
    "${C_SRC}/elitte/interpreter.cpp"
    "${C_SRC}/elitte/metadata.cpp"
    "${C_SRC}/elitte/interpreter_builder.cpp"
    "${C_SRC}/elitte/ops/builtin/builtin_resolver.cpp"
    "${C_SRC}/elitte/signature_runner.cpp"
    "${C_SRC}/elitte/status.cpp"
    "${C_SRC}/elitte/tflitetensor.cpp"
    "${C_SRC}/elitte/tflite.cpp"
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

# LiteRT's own C API, the layer it adds above the tflite subtree we bind. Its
# top level CMakeLists adds tflite itself, which would be the second time in this
# build, so the c directory is added on its own once tensorflow-lite exists.
option(TFLITE_BEAM_ENABLE_LITERT_API "Build LiteRT's C API alongside the tflite one" OFF)
if(TFLITE_BEAM_ENABLE_LITERT_API)
    # LiteRT will build without XNNPACK and then not load. Its CPU accelerator
    # is XNNPACK: RegisterCpuAccelerator is defined in
    # litert/runtime/accelerators/xnnpack/xnnpack_accelerator.cc, which
    # litert/runtime/CMakeLists.txt compiles unconditionally, so the .so links
    # with TfLiteXNNPackDelegate* undefined. Measured on a Pi Zero W: it builds,
    # ships, and fails on_load with nothing pointing at the cause.
    #
    # armv6 and armv7l are the shapes that turn XNNPACK off, and neither has a
    # usable GPU either, so there is no accelerator left for LiteRT to reach.
    # Saying so here beats emitting an artifact that cannot be loaded.
    # DEFINED matters: only the armv6 and armv7l branches above set this, and on
    # every other target TFLite decides it later, from inside its own subtree.
    # Testing the value alone would read "not set" as "off" and refuse the build
    # on x86_64 and arm64, which is every platform that can have LiteRT at all.
    if(DEFINED TFLITE_ENABLE_XNNPACK AND NOT TFLITE_ENABLE_XNNPACK)
        message(FATAL_ERROR
            "TFLITE_BEAM_ENABLE_LITERT_API needs XNNPACK, and this target "
            "builds with it off. LiteRT's CPU accelerator is XNNPACK, so the "
            "library would link with TfLiteXNNPackDelegate* undefined and fail "
            "to load. Build without the LiteRT API on this target.")
    endif()
    set(LITERT_SOURCE_DIR "${LITERT_ROOT_DIR}" CACHE PATH "LiteRT source root")
    # litert/core/model reads the converter schema out of this; normally set by
    # litert's top level, which also uses it to add tflite
    set(TFLITE_SOURCE_DIR "${TFLITE_ROOT_DIR}" CACHE PATH "TFLite source directory")
    set(TFLITE_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}/tflite_runtime" CACHE PATH "TFLite build directory")
    # All three of these come from litert's own top level CMakeLists, which is not
    # used here because it would add the tflite subtree a second time.
    find_package(Threads REQUIRED)
    if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
        set(LITERT_PLATFORM_MACOS ON)
    elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
        set(LITERT_PLATFORM_LINUX ON)
    elseif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
        set(LITERT_PLATFORM_WINDOWS ON)
    elseif(CMAKE_SYSTEM_NAME STREQUAL "Android")
        set(LITERT_PLATFORM_ANDROID ON)
    endif()
    if(NOT DEFINED CMAKE_HOST_EXECUTABLE_SUFFIX)
        if(CMAKE_HOST_WIN32)
            set(CMAKE_HOST_EXECUTABLE_SUFFIX ".exe")
        else()
            set(CMAKE_HOST_EXECUTABLE_SUFFIX "")
        endif()
    endif()

    # build_config.h is generated from a template and reached as
    # "litert/build_common/build_config.h", so the generated tree needs that shape.
    set(LITERT_GENERATED_INCLUDE_DIR "${CMAKE_CURRENT_BINARY_DIR}/litert_generated")
    set(LITERT_BUILD_COMMON_GENERATED_DIR "${LITERT_GENERATED_INCLUDE_DIR}/litert/build_common")
    file(MAKE_DIRECTORY "${LITERT_BUILD_COMMON_GENERATED_DIR}")
    # GPU follows the LiteRT API rather than being a second thing to ask for.
    # With it off, RegisterGpuAccelerator compiles to a stub, so a caller who
    # names the GPU gets a silent fall back to the CPU and nothing to explain
    # it, which is the whole feature missing rather than a tuning choice. The
    # OpenCL headers are fetched by the module below rather than expected from
    # the system, and the accelerator itself is a plugin this library dlopens,
    # so turning it on costs the build very little. -DLITERT_ENABLE_GPU=OFF
    # still turns it back off.
    #
    # NPU stays off: it wants vendor SDKs that cannot be fetched.
    option(LITERT_ENABLE_GPU "Enable GPU acceleration support" ON)
    option(LITERT_ENABLE_NPU "Enable NPU acceleration support" OFF)
    if(LITERT_ENABLE_GPU)
        set(LITERT_BUILD_CONFIG_DISABLE_GPU 0)
    else()
        set(LITERT_BUILD_CONFIG_DISABLE_GPU 1)
    endif()
    if(LITERT_ENABLE_NPU)
        set(LITERT_BUILD_CONFIG_DISABLE_NPU 0)
    else()
        set(LITERT_BUILD_CONFIG_DISABLE_NPU 1)
    endif()
    configure_file(
        "${LITERT_ROOT_DIR}/litert/build_common/build_config.h.in"
        "${LITERT_BUILD_COMMON_GENERATED_DIR}/build_config.h"
    )
    include_directories("${LITERT_GENERATED_INCLUDE_DIR}" "${LITERT_ROOT_DIR}")

    add_compile_definitions(CL_TARGET_OPENCL_VERSION=300)

    # LiteRT 2.4.0's core and runtime targets compile against ML Drift and
    # Khronos headers (OpenCL/Vulkan/OpenGL/EGL), fetched via TFLite's CMake
    # modules rather than expected from the system.
    list(APPEND CMAKE_MODULE_PATH "${TFLITE_ROOT_DIR}/tools/cmake/modules")
    if(NOT EXISTS "${CMAKE_BINARY_DIR}/opencl_headers")
        find_package(opencl_headers REQUIRED)
    endif()
    include_directories("${CMAKE_BINARY_DIR}/opencl_headers")

    if(NOT EXISTS "${CMAKE_BINARY_DIR}/vulkan_headers")
        find_package(vulkan_headers REQUIRED)
    endif()
    include_directories(AFTER "${CMAKE_BINARY_DIR}/vulkan_headers/include")

    if(NOT EXISTS "${CMAKE_BINARY_DIR}/opengl_headers")
        find_package(opengl_headers REQUIRED)
    endif()
    include_directories(AFTER "${CMAKE_BINARY_DIR}/opengl_headers/api")

    if(NOT "${CMAKE_SYSTEM_NAME}" STREQUAL "Android")
        if(NOT EXISTS "${CMAKE_BINARY_DIR}/egl_headers")
            find_package(egl_headers REQUIRED)
        endif()
        include_directories(AFTER "${CMAKE_BINARY_DIR}/egl_headers/api")
    endif()

    # XNNPACK already fetched FP16 into this build, so point at it rather than
    # fetching a second copy.
    if(EXISTS "${CMAKE_CURRENT_BINARY_DIR}/FP16-source/include")
        include_directories("${CMAKE_CURRENT_BINARY_DIR}/FP16-source/include")
    elseif(NOT EXISTS "${CMAKE_BINARY_DIR}/fp16_headers")
        find_package(fp16_headers REQUIRED)
    else()
        include_directories("${CMAKE_BINARY_DIR}/fp16_headers/include")
    endif()
    # litert/cc/litert_buffer_ref.h declares members named Span whose return type
    # is the Span alias already used earlier in the same class. GCC rejects that
    # and Clang does not, so every Linux build with the default toolchain fails on
    # a header we do not own. The diagnostic is tagged -fpermissive up to GCC 13
    # and became its own -Wchanges-meaning in 14. Scoped to the subtree that needs
    # it by saving the flags and restoring them after: -fpermissive is broad
    # enough that it has no business reaching our own sources.
    set(_litert_saved_cxx_flags "${CMAKE_CXX_FLAGS}")
    if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
        if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL 14)
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-changes-meaning")
        else()
            set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fpermissive")
        endif()
    endif()

    # Keep LiteRT's own OpenCL buffer interop compiled out so the host stays free
    # of an OpenCL runtime dependency; the GPU accelerator plugin brings its own
    # OpenCL and libLiteRtGpuAccelerator is unaffected.
    add_compile_definitions(LITERT_DISABLE_OPENCL_SUPPORT)

    include(OverridableFetchContent)
    OverridableFetchContent_Declare(
        safeint
        GIT_REPOSITORY https://github.com/dcleblanc/SafeInt
        GIT_TAG 72f0745e26091af6d1186fd91426377f16688ba7
        GIT_PROGRESS TRUE
        PREFIX "${CMAKE_BINARY_DIR}"
        SOURCE_DIR "${CMAKE_BINARY_DIR}/safeint"
    )
    OverridableFetchContent_GetProperties(safeint)
    if(NOT safeint_POPULATED)
        OverridableFetchContent_Populate(safeint)
    endif()
    include_directories("${safeint_SOURCE_DIR}")

    OverridableFetchContent_Declare(
        ml_drift
        GIT_REPOSITORY https://github.com/google-ai-edge/ml-drift.git
        GIT_TAG 1b18771f44c0be3d972847b31a80f6dc3d6964b5
        GIT_PROGRESS TRUE
        PREFIX "${CMAKE_BINARY_DIR}"
        SOURCE_DIR "${CMAKE_BINARY_DIR}/ml_drift"
    )
    OverridableFetchContent_GetProperties(ml_drift)
    if(NOT ml_drift_POPULATED)
        OverridableFetchContent_Populate(ml_drift)
    endif()
    include_directories("${ml_drift_SOURCE_DIR}")

    if(NOT TARGET flatbuffers::flatbuffers AND TARGET flatbuffers)
        add_library(flatbuffers::flatbuffers ALIAS flatbuffers)
    endif()

    set(LITERT_FLATC_EXECUTABLE "")
    set(LITERT_FLATC_TARGET "")
    if(CMAKE_CROSSCOMPILING)
        if(TFLITE_HOST_TOOLS_DIR)
            foreach(_litert_flatc_candidate
                    "${TFLITE_HOST_TOOLS_DIR}/flatc${CMAKE_HOST_EXECUTABLE_SUFFIX}"
                    "${TFLITE_HOST_TOOLS_DIR}/Release/flatc${CMAKE_HOST_EXECUTABLE_SUFFIX}"
                    "${TFLITE_HOST_TOOLS_DIR}/bin/flatc${CMAKE_HOST_EXECUTABLE_SUFFIX}"
                    "${TFLITE_HOST_TOOLS_DIR}/flatbuffers-flatc/bin/flatc${CMAKE_HOST_EXECUTABLE_SUFFIX}")
                if(EXISTS "${_litert_flatc_candidate}")
                    set(LITERT_FLATC_EXECUTABLE "${_litert_flatc_candidate}")
                    break()
                endif()
            endforeach()
        endif()
        if(NOT LITERT_FLATC_EXECUTABLE)
            unset(LITERT_FLATC_EXECUTABLE)
            find_program(LITERT_FLATC_EXECUTABLE NAMES flatc NO_CMAKE_FIND_ROOT_PATH)
        endif()
        if(NOT LITERT_FLATC_EXECUTABLE AND EXISTS "${CMAKE_BINARY_DIR}/flatbuffers-flatc/bin/flatc${CMAKE_HOST_EXECUTABLE_SUFFIX}")
            set(LITERT_FLATC_EXECUTABLE "${CMAKE_BINARY_DIR}/flatbuffers-flatc/bin/flatc${CMAKE_HOST_EXECUTABLE_SUFFIX}")
        endif()
    else()
        if(TARGET flatbuffers::flatc)
            set(LITERT_FLATC_EXECUTABLE "$<TARGET_FILE:flatbuffers::flatc>")
            set(LITERT_FLATC_TARGET flatbuffers::flatc)
        elseif(TARGET flatc)
            set(LITERT_FLATC_EXECUTABLE "$<TARGET_FILE:flatc>")
            set(LITERT_FLATC_TARGET flatc)
        elseif(EXISTS "${CMAKE_BINARY_DIR}/flatbuffers-flatc/bin/flatc${CMAKE_HOST_EXECUTABLE_SUFFIX}")
            set(LITERT_FLATC_EXECUTABLE "${CMAKE_BINARY_DIR}/flatbuffers-flatc/bin/flatc${CMAKE_HOST_EXECUTABLE_SUFFIX}")
        elseif(EXISTS "${CMAKE_BINARY_DIR}/_deps/flatbuffers-build/flatc${CMAKE_HOST_EXECUTABLE_SUFFIX}")
            set(LITERT_FLATC_EXECUTABLE "${CMAKE_BINARY_DIR}/_deps/flatbuffers-build/flatc${CMAKE_HOST_EXECUTABLE_SUFFIX}")
        else()
            unset(LITERT_FLATC_EXECUTABLE)
            find_program(LITERT_FLATC_EXECUTABLE NAMES flatc NO_CMAKE_FIND_ROOT_PATH)
        endif()
    endif()
    if(NOT LITERT_FLATC_EXECUTABLE)
        message(FATAL_ERROR
            "Could not locate flatc to generate ML Drift FlatBuffer headers. "
            "Install flatc or set TFLITE_HOST_TOOLS_DIR.")
    endif()

    set(MLDRIFT_SERIALIZATION_BASE_FBS "${ml_drift_SOURCE_DIR}/ml_drift/common/task/serialization_base.fbs")
    set(MLDRIFT_SERIALIZATION_BASE_H "${ml_drift_SOURCE_DIR}/ml_drift/common/task/serialization_base_generated.h")
    set(MLDRIFT_CL_PROGRAM_CACHE_FBS "${ml_drift_SOURCE_DIR}/ml_drift/cl/compiled_program_cache.fbs")
    set(MLDRIFT_CL_PROGRAM_CACHE_H "${ml_drift_SOURCE_DIR}/ml_drift/cl/compiled_program_cache_generated.h")
    add_custom_command(
        OUTPUT "${MLDRIFT_SERIALIZATION_BASE_H}" "${MLDRIFT_CL_PROGRAM_CACHE_H}"
        COMMAND ${LITERT_FLATC_EXECUTABLE} --cpp --scoped-enums
                -I "${ml_drift_SOURCE_DIR}"
                -o "${ml_drift_SOURCE_DIR}/ml_drift/common/task"
                "${MLDRIFT_SERIALIZATION_BASE_FBS}"
        COMMAND ${LITERT_FLATC_EXECUTABLE} --cpp --scoped-enums
                -I "${ml_drift_SOURCE_DIR}"
                -o "${ml_drift_SOURCE_DIR}/ml_drift/cl"
                "${MLDRIFT_CL_PROGRAM_CACHE_FBS}"
        DEPENDS "${MLDRIFT_SERIALIZATION_BASE_FBS}" "${MLDRIFT_CL_PROGRAM_CACHE_FBS}" ${LITERT_FLATC_TARGET}
        COMMENT "Generating ML Drift FlatBuffer headers"
    )
    add_custom_target(mldrift_serialization_gen DEPENDS
        "${MLDRIFT_SERIALIZATION_BASE_H}"
        "${MLDRIFT_CL_PROGRAM_CACHE_H}"
    )

    # The C API sits on cc/internal, which sits on core. litert's own top level
    # adds these in the same order; only its add_subdirectory of tflite is left
    # out, since this build already has one.
    add_subdirectory(
        "${LITERT_ROOT_DIR}/litert/core"
        "${CMAKE_CURRENT_BINARY_DIR}/litert_core"
        EXCLUDE_FROM_ALL
    )
    add_subdirectory(
        "${LITERT_ROOT_DIR}/litert/compiler"
        "${CMAKE_CURRENT_BINARY_DIR}/litert_compiler"
        EXCLUDE_FROM_ALL
    )
    add_subdirectory(
        "${LITERT_ROOT_DIR}/litert/runtime"
        "${CMAKE_CURRENT_BINARY_DIR}/litert_runtime"
        EXCLUDE_FROM_ALL
    )
    add_subdirectory(
        "${LITERT_ROOT_DIR}/litert/cc"
        "${CMAKE_CURRENT_BINARY_DIR}/litert_cc"
        EXCLUDE_FROM_ALL
    )
    add_subdirectory(
        "${LITERT_ROOT_DIR}/litert/c"
        "${CMAKE_CURRENT_BINARY_DIR}/litert_c"
        EXCLUDE_FROM_ALL
    )
    set(CMAKE_CXX_FLAGS "${_litert_saved_cxx_flags}")

    target_sources(tflite_beam PRIVATE
        "${CMAKE_CURRENT_SOURCE_DIR}/c_src/elitte/litert_compiled_model.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/c_src/elitte/litert_backend_options.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/c_src/elitte/litert_model.cpp"
        "${CMAKE_CURRENT_SOURCE_DIR}/c_src/elitte/litert_reaper.cpp")
    # the compiled model path reaches absl's CHECK macros, which live in
    # absl::log_internal_check_op and are not among the absl targets the tflite
    # side needs
    list(APPEND TFLITE_BEAM_LINK_LIBRARIES litert_c_api absl::log_internal_check_op absl::log)
    add_compile_definitions(TFLITE_BEAM_LITERT_API_ENABLED=1)
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
    target_include_directories(tflite_beam_test_delegate PRIVATE "${LITERT_ROOT_DIR}")
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
    CXX_VISIBILITY_PRESET hidden
    VISIBILITY_INLINES_HIDDEN TRUE
)

if(UNIX AND NOT APPLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-but-set-variable")
    set_target_properties(tflite_beam PROPERTIES INSTALL_RPATH "\$ORIGIN/libedgetpu")
elseif(UNIX AND APPLE)
    target_link_options(tflite_beam PRIVATE "-undefined" "dynamic_lookup")
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
if(TFLITE_BEAM_LITERT_VERSION)
    add_compile_definitions(TFLITE_BEAM_LITERT_VERSION="${TFLITE_BEAM_LITERT_VERSION}")
endif()
if(TFLITE_ENABLE_XNNPACK)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DTFLITE_BEAM_XNNPACK_ENABLED=1")
    # XNN_MAX_TENSOR_DIMS bounds every reshape the delegate performs, and the
    # binding refuses a resize that would carry a tensor across it. Lift the
    # value out of XNNPACK's own header so the guard cannot drift from what the
    # delegate was compiled with. Read rather than included: xnnpack.h pulls in
    # pthreadpool.h, and it is only on the include path here by way of TfLite
    # linking XNNPACK, which is not a thing to depend on for a constant.
    if(NOT XNNPACK_SOURCE_DIR)
        message(FATAL_ERROR "XNNPACK is enabled but XNNPACK_SOURCE_DIR is unset, so "
                            "XNN_MAX_TENSOR_DIMS cannot be read from its header")
    endif()
    file(STRINGS "${XNNPACK_SOURCE_DIR}/include/xnnpack.h" XNN_MAX_DIMS_LINE
         REGEX "^#define[ \t]+XNN_MAX_TENSOR_DIMS[ \t]+[0-9]+")
    if(NOT XNN_MAX_DIMS_LINE)
        message(FATAL_ERROR "XNN_MAX_TENSOR_DIMS is not where it was in "
                            "${XNNPACK_SOURCE_DIR}/include/xnnpack.h, so the resize "
                            "guard has nothing to enforce")
    endif()
    string(REGEX REPLACE "^#define[ \t]+XNN_MAX_TENSOR_DIMS[ \t]+([0-9]+).*" "\\1"
           TFLITE_BEAM_MAX_DELEGATED_RANK "${XNN_MAX_DIMS_LINE}")
    message(STATUS "XNN_MAX_TENSOR_DIMS is ${TFLITE_BEAM_MAX_DELEGATED_RANK}")
    set(CMAKE_CXX_FLAGS
        "${CMAKE_CXX_FLAGS} -DTFLITE_BEAM_MAX_DELEGATED_RANK=${TFLITE_BEAM_MAX_DELEGATED_RANK}")
endif()
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -g3")
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
endif()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wno-unused-function -Wno-sign-compare -Wno-unused-parameter -Wno-missing-field-initializers -Wno-deprecated-declarations")
