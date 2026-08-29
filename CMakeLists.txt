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

# LiteRT fetches Eigen by cloning gitlab.com, which throttles under load and has
# refused this build twice. The mirror is a plain copy carrying the same commit,
# so the pin still decides which Eigen is used and only the host changes.
# OverridableFetchContent reads this name for the content it calls "eigen"; see
# tflite/tools/cmake/modules/OverridableFetchContent.cmake.
if(NOT OVERRIDABLE_FETCH_CONTENT_eigen_GIT_REPOSITORY)
    set(OVERRIDABLE_FETCH_CONTENT_eigen_GIT_REPOSITORY
        "https://github.com/cocoa-xu/eigen.git" CACHE STRING
        "Where to fetch Eigen from; the commit is still pinned by LiteRT")
endif()

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
    set(LITERT_SOURCE_DIR "${LITERT_ROOT_DIR}" CACHE PATH "LiteRT source root")
    # litert/core/model reads the converter schema out of this; normally set by
    # litert's top level, which also uses it to add tflite
    set(TFLITE_SOURCE_DIR "${TFLITE_ROOT_DIR}" CACHE PATH "TFLite source directory")
    set(TFLITE_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}/tflite_runtime" CACHE PATH "TFLite build directory")
    # All three of these come from litert's own top level CMakeLists, which is not
    # used here because it would add the tflite subtree a second time.
    find_package(Threads REQUIRED)

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

    # litert/runtime compiles open_cl_memory.cc and open_cl_sync.cc unconditionally,
    # with no option gating them, so CL/cl.h is needed whether or not the GPU is
    # wanted. TFLite ships the module that fetches those headers; this borrows it.
    list(APPEND CMAKE_MODULE_PATH "${TFLITE_ROOT_DIR}/tools/cmake/modules")
    include(opencl_headers)

    # and those files reach delegates/gpu/common/types.h, which wants fp16.h.
    # XNNPACK already fetched FP16 into this build, so point at it rather than
    # fetching a second copy.
    if(EXISTS "${CMAKE_CURRENT_BINARY_DIR}/FP16-source/include")
        include_directories("${CMAKE_CURRENT_BINARY_DIR}/FP16-source/include")
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

    # litert/runtime's event.cc, gpu_environment.cc and open_cl_memory.cc call
    # TFLite's own OpenCL layer, which no target here builds, so off Apple the
    # .so links with 22 symbols missing and fails to load. That layer exists so
    # LiteRT can pass CL buffers around itself, and nothing here asks it to: the
    # GPU accelerator is a plugin that brings its own OpenCL. Compiling it out
    # keeps the host free of a dependency it does not use, and the accelerator is
    # still found, because the name the registry tries first,
    # libLiteRtGpuAccelerator, is not one of the ones this gates.
    add_compile_definitions(LITERT_DISABLE_OPENCL_SUPPORT)

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
