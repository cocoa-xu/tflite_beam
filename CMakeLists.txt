cmake_minimum_required(VERSION 3.16 FATAL_ERROR)
project(tflite_elixir C CXX)

set(TFLITE_ROOT_DIR "" CACHE PATH
    "Directory that contains the TensorFlow Lite project"
)
if(NOT TFLITE_ROOT_DIR)
    message(FATAL_ERROR "TFLITE_ROOT_DIR is not set.")
endif()

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")

add_subdirectory(
    "${TFLITE_ROOT_DIR}"
    EXCLUDE_FROM_ALL
)

if(${TFLITE_ELIXIR_CORAL_SUPPORT})
    set(GFLAGS_ROOT_DIR "" CACHE PATH
        "Directory that contains the gflags project"
    )
    if(NOT GFLAGS_ROOT_DIR)
        message(FATAL_ERROR "GFLAGS_ROOT_DIR is not set.")
    endif()

    add_subdirectory(
        "${GFLAGS_ROOT_DIR}"
        EXCLUDE_FROM_ALL
    )

    set(BUILD_SHARED_LIBS OFF)
    set(GLOG_ROOT_DIR "" CACHE PATH
        "Directory that contains the glog project"
    )
    if(NOT GLOG_ROOT_DIR)
        message(FATAL_ERROR "GLOG_ROOT_DIR is not set.")
    endif()

    add_subdirectory(
        "${GLOG_ROOT_DIR}"
        EXCLUDE_FROM_ALL
    )
endif()

set(SOURCE_FILES
    "${C_SRC}/helper.cpp"
    "${C_SRC}/nif_utils.cpp"
    "${C_SRC}/tflite/tflite_tflitetensor.cpp"
    "${C_SRC}/tflite/tflite_interpreter.cpp"
    "${C_SRC}/bindings.cpp"
)
if(${TFLITE_ELIXIR_CORAL_SUPPORT})
    include_directories("${C_SRC}/libcoral")
    include_directories("${C_SRC}/libcoral/libedgetpu")

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
endif()

add_library(tflite_elixir SHARED
    ${SOURCE_FILES}
)
set(TFLITE_ELIXIR_LINK_LIBRARIES "tensorflow-lite")
if(${TFLITE_ELIXIR_CORAL_SUPPORT})
    if(UNIX AND NOT APPLE)
        list(APPEND TFLITE_ELIXIR_LINK_LIBRARIES "edgetpu")
        target_link_directories(tflite_elixir PUBLIC "${PRIV_DIR}/libedgetpu")
    elseif(UNIX AND APPLE)
        list(APPEND TFLITE_ELIXIR_LINK_LIBRARIES "edgetpu.1.0")
    endif()
    list(APPEND TFLITE_ELIXIR_LINK_LIBRARIES "gflags")
    list(APPEND TFLITE_ELIXIR_LINK_LIBRARIES "glog")
endif()

target_link_libraries(tflite_elixir
    ${TFLITE_ELIXIR_LINK_LIBRARIES}
)
set_target_properties(tflite_elixir PROPERTIES PREFIX "")
set_target_properties(tflite_elixir PROPERTIES SUFFIX ".so")
set_target_properties(tflite_elixir PROPERTIES
    INSTALL_RPATH_USE_LINK_PATH TRUE
    BUILD_WITH_INSTALL_RPATH TRUE
)

if(UNIX AND NOT APPLE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -shared")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-unused-but-set-variable")
    set_target_properties(tflite_elixir PROPERTIES INSTALL_RPATH "\$ORIGIN/libedgetpu")
elseif(UNIX AND APPLE)
    set(CMAKE_SHARED_LINKER_FLAGS "-flat_namespace -undefined suppress -undefined dynamic_lookup")
    set_target_properties(tflite_elixir PROPERTIES INSTALL_RPATH "@loader_path/libedgetpu")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC -I${ERTS_INCLUDE_DIR}")
if(${TFLITE_ELIXIR_CORAL_SUPPORT})
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -L${PRIV_DIR}/libedgetpu")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DCORAL_SUPPORT_ENABLED=1")
endif()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3 -Wall -Wno-unused-function -Wno-sign-compare -Wno-unused-parameter -Wno-missing-field-initializers -Wno-deprecated-declarations")
