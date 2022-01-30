PRIV_DIR = $(MIX_APP_PATH)/priv
NATIVE_BINDINGS_SO = $(PRIV_DIR)/tflite_elixir.so
C_SRC = $(shell pwd)/c_src
LIB_SRC = $(shell pwd)/lib
ifdef CMAKE_TOOLCHAIN_FILE
	CMAKE_CONFIGURE_FLAGS=-D CMAKE_TOOLCHAIN_FILE="$(CMAKE_TOOLCHAIN_FILE)"
endif

# Tensorflow
TFLITE_USE_GIT_HEAD ?= false
TFLITE_GIT_REPO ?= "https://github.com/tensorflow/tensorflow.git"
TFLITE_VER ?= 2.7.0
TFLITE_VER_V = v$(TFLITE_VER)
ifneq ($(TFLITE_USE_GIT_HEAD), false)
	TFLITE_VER_V=$(TFLITE_USE_GIT_BRANCH)
endif
TFLITE_CACHE_DIR = $(shell pwd)/3rd_party/cache
TFLITE_SOURCE_URL = "https://github.com/tensorflow/tensorflow/archive/refs/tags/$(TFLITE_VER_V).zip"
TFLITE_SOURCE_ZIP = $(TFLITE_CACHE_DIR)/tensorflow-$(TFLITE_VER_V).zip
TENSORFLOW_ROOT_DIR = $(shell pwd)/3rd_party/tensorflow/tensorflow-$(TFLITE_VER)
TFLITE_ROOT_DIR = $(TENSORFLOW_ROOT_DIR)/tensorflow/lite
TFLITE_CMAKELISTS_TXT = $(TFLITE_ROOT_DIR)/CMakeLists.txt
CMAKE_TFLITE_BUILD_DIR = $(MIX_APP_PATH)/cmake_tflite_$(TFLITE_VER)

CMAKE_TFLITE_OPTIONS ?= ""
CMAKE_OPTIONS ?= $(CMAKE_TFLITE_OPTIONS)
CMAKE_OPTIONS += $(CMAKE_CONFIGURE_FLAGS)

# bindings
CMAKE_BINDINGS_BUILD_DIR = $(MIX_APP_PATH)/cmake_tflite_elixir
MAKE_BUILD_FLAGS ?= "-j1"

.DEFAULT_GLOBAL := build

build: $(NATIVE_BINDINGS_SO)

$(TFLITE_SOURCE_ZIP):
	@ mkdir -p "$(TFLITE_CACHE_DIR)"
	@ if [ "$(TFLITE_USE_GIT_HEAD)" = "false" ] && [ ! -e "$(TFLITE_SOURCE_ZIP)" ]; then \
		if [ -e "$(shell which curl)" ]; then \
			curl -fSL "$(TFLITE_SOURCE_URL)" -o $(TFLITE_SOURCE_ZIP) ; \
		elif [ -e "$(shell which wget)" ]; then \
			wget "$(TFLITE_SOURCE_URL)" -O $(TFLITE_SOURCE_ZIP) ; \
		else \
			echo "cannot find curl or wget, cannot download tensorflow source code" ; \
			exit 1 ; \
		fi \
	fi

unarchive_source_code: $(TFLITE_SOURCE_ZIP)
	@ if [ ! -e "$(TFLITE_CMAKELISTS_TXT)" ]; then \
		rm -rf "$(TFLITE_DIR)" ; \
    		if [ "$(TFLITE_USE_GIT_HEAD)" = "false" ]; then \
    			unzip -qq -o "$(TFLITE_SOURCE_ZIP)" -d "$(TENSORFLOW_ROOT_DIR)" ; \
    		else \
    			git clone --branch=$(TFLITE_USE_GIT_BRANCH) --depth=1 $(TFLITE_GIT_REPO) "$(TFLITE_DIR)" ; \
    		fi \
    	fi

$(NATIVE_BINDINGS_SO): unarchive_source_code
	@ mkdir -p $(PRIV_DIR)
	@ mkdir -p $(CMAKE_BINDINGS_BUILD_DIR)
	@ cd "$(CMAKE_BINDINGS_BUILD_DIR)" && \
 		{ cmake -D C_SRC="$(C_SRC)" \
 		  -D PRIV_DIR="$(PRIV_DIR)" \
 		  -D ERTS_INCLUDE_DIR="$(ERTS_INCLUDE_DIR)" \
 		  -D TFLITE_ROOT_DIR="$(TFLITE_ROOT_DIR)" \
 		  $(CMAKE_OPTIONS) \
 		  "$(shell pwd)" && \
 		  make "$(MAKE_BUILD_FLAGS)" \
 		  || { echo "\033[0;31mincomplete build of Tensorflow Lite found in '$(CMAKE_TFLITE_BUILD_DIR)', please delete that directory and retry\033[0m" && exit 1 ; } ; } \
 		&& cp "$(CMAKE_BINDINGS_BUILD_DIR)/tflite_elixir.so" "$(NATIVE_BINDINGS_SO)"
