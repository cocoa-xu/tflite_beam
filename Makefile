PRIV_DIR = $(MIX_APP_PATH)/priv
NATIVE_BINDINGS_SO = $(PRIV_DIR)/tflite_elixir.so
LIBEDGETPU_RUNTIME_PRIV = $(PRIV_DIR)/libedgetpu
TFLITE_ELIXIR_ONLY_COPY_PRIV ?= "NO"
SCRIPTS_DIR = $(shell pwd)/scripts
C_SRC = $(shell pwd)/c_src
LIB_SRC = $(shell pwd)/lib
ifdef CMAKE_TOOLCHAIN_FILE
	CMAKE_CONFIGURE_FLAGS=-D CMAKE_TOOLCHAIN_FILE="$(CMAKE_TOOLCHAIN_FILE)"
endif

# Tensorflow
TFLITE_USE_GIT_HEAD ?= false
TFLITE_GIT_REPO ?= "https://github.com/tensorflow/tensorflow.git"
TFLITE_VER ?= 2.11.0
TFLITE_VER_V = v$(TFLITE_VER)
ifneq ($(TFLITE_USE_GIT_HEAD), false)
	TFLITE_VER_V=$(TFLITE_USE_GIT_BRANCH)
endif
THIRD_PARTY_DIR = $(shell pwd)/3rd_party
TFLITE_ELIXIR_CACHE_DIR = $(THIRD_PARTY_DIR)/cache
TFLITE_SOURCE_URL = "https://github.com/tensorflow/tensorflow/archive/refs/tags/$(TFLITE_VER_V).zip"
TFLITE_SOURCE_ZIP = $(TFLITE_ELIXIR_CACHE_DIR)/tensorflow-$(TFLITE_VER_V).zip
UNZIP_TARGET_DIR = $(THIRD_PARTY_DIR)/tensorflow
TENSORFLOW_ROOT_DIR = $(UNZIP_TARGET_DIR)/tensorflow-$(TFLITE_VER)
TFLITE_ROOT_DIR = $(TENSORFLOW_ROOT_DIR)/tensorflow/lite
GFLAGS_ROOT_DIR = $(THIRD_PARTY_DIR)/gflags
GLOG_ROOT_DIR = $(THIRD_PARTY_DIR)/glog
TFLITE_CMAKELISTS_TXT = $(TFLITE_ROOT_DIR)/CMakeLists.txt
CMAKE_TFLITE_BUILD_DIR = $(MIX_APP_PATH)/cmake_tflite_$(TFLITE_VER)

LIBUSB_VERSION = 1.0.26
LIBUSB_SOURCE_URL = https://github.com/libusb/libusb/releases/download/v$(LIBUSB_VERSION)/libusb-$(LIBUSB_VERSION).tar.bz2
LIBUSB_SOURCE_ARCHIVE = $(TFLITE_ELIXIR_CACHE_DIR)/libusb-$(LIBUSB_VERSION).tar.bz2
LIBUSB_SOURCE_DIR = $(THIRD_PARTY_DIR)/libusb-$(LIBUSB_VERSION)
LIBUSB_INSTALL_DIR = $(MIX_APP_PATH)/libusb
LIBUSB_SHARED_LIBRARY = $(PRIV_DIR)/libedgetpu/libusb-1.0.0.dylib
LIBUSB_CONFIGURE_AC = $(shell pwd)/patches/libusb-configure.ac

UNAME_S := $(shell uname -s)
ifneq ($(UNAME_S),Darwin)
LIBUSB_SHARED_LIBRARY = $(PRIV_DIR)/libedgetpu/libusb-1.0.so.0.3.0
endif
ifeq ($(TARGET_OS),linux)
LIBUSB_SHARED_LIBRARY = $(PRIV_DIR)/libedgetpu/libusb-1.0.so.0.3.0
endif

TFLITE_ELIXIR_CORAL_USB_THROTTLE ?= YES
TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET ?= native
TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR = $(TFLITE_ELIXIR_CACHE_DIR)/$(TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME)

CMAKE_TFLITE_OPTIONS ?= ""
CMAKE_OPTIONS ?= $(CMAKE_TFLITE_OPTIONS)
CMAKE_OPTIONS += $(CMAKE_CONFIGURE_FLAGS)
ifeq ($(TARGET_ARCH),arm)
CMAKE_OPTIONS += -D TFLITE_ENABLE_XNNPACK=OFF
endif

# bindings
CMAKE_BINDINGS_BUILD_DIR = $(MIX_APP_PATH)/cmake_tflite_elixir
MAKE_BUILD_FLAGS ?= "-j1"

.DEFAULT_GLOBAL := build

build: $(NATIVE_BINDINGS_SO) fix_libusb

create_cache_dir:
	@ mkdir -p "$(TFLITE_ELIXIR_CACHE_DIR)"

$(TFLITE_SOURCE_ZIP): create_cache_dir
	@ if [ "$(TFLITE_ELIXIR_ONLY_COPY_PRIV)" = "NO" ]; then \
		if [ "$(TFLITE_USE_GIT_HEAD)" = "false" ] && [ ! -e "$(TFLITE_SOURCE_ZIP)" ]; then \
			if [ -e "$(shell which curl)" ]; then \
				curl -fSL "$(TFLITE_SOURCE_URL)" -o $(TFLITE_SOURCE_ZIP) ; \
			elif [ -e "$(shell which wget)" ]; then \
				wget "$(TFLITE_SOURCE_URL)" -O $(TFLITE_SOURCE_ZIP) ; \
			else \
				echo "cannot find curl or wget, cannot download tensorflow source code" ; \
				exit 1 ; \
			fi \
		fi \
	fi

unarchive_source_code: $(TFLITE_SOURCE_ZIP)
	@ if [ "$(TFLITE_ELIXIR_ONLY_COPY_PRIV)" = "NO" ]; then \
   		if [ ! -d "$(TENSORFLOW_ROOT_DIR)" ]; then \
			rm -rf "$(TENSORFLOW_ROOT_DIR)" ; \
			mkdir -p "$(UNZIP_TARGET_DIR)" ; \
			if [ "$(TFLITE_USE_GIT_HEAD)" = "false" ]; then \
				unzip -qq -o "$(TFLITE_SOURCE_ZIP)" -d "$(UNZIP_TARGET_DIR)" ; \
			else \
				git clone --branch=$(TFLITE_USE_GIT_BRANCH) --depth=1 $(TFLITE_GIT_REPO) "$(TENSORFLOW_ROOT_DIR)" ; \
			fi \
		fi \
	fi

install_libedgetpu_runtime:
	@ if [ "$(TFLITE_ELIXIR_ONLY_COPY_PRIV)" = "NO" ]; then \
   		if [ "$(TFLITE_ELIXIR_CORAL_SUPPORT)" = "YES" ]; then \
			bash scripts/copy_libedgetpu_runtime.sh "$(LIBEDGETPU_RUNTIME_PRIV)" "$(TFLITE_ELIXIR_CORAL_LIBEDGETPU_UNZIPPED_DIR)" "$(TFLITE_ELIXIR_CORAL_LIBEDGETPU_TRIPLET)" "$(TFLITE_ELIXIR_CORAL_USB_THROTTLE)" "$(TFLITE_ELIXIR_CORAL_LIBEDGETPU_URL)" "$(TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME)" "$(TFLITE_ELIXIR_CACHE_DIR)" && \
			git submodule update --init c_src/libcoral && \
			cd c_src/libcoral && git submodule update --init libedgetpu && cd ../.. ; \
		fi \
	fi

libusb: create_cache_dir
	@ if [ "$(TFLITE_ELIXIR_ONLY_COPY_PRIV)" = "NO" ]; then \
		if [ "$(TFLITE_ELIXIR_CORAL_SUPPORT)" = "YES" ]; then \
			if [ ! -e "$(LIBUSB_SHARED_LIBRARY)" ]; then \
				bash scripts/build_libusb.sh "$(LIBUSB_SOURCE_URL)" "$(LIBUSB_SOURCE_ARCHIVE)" "$(THIRD_PARTY_DIR)" "$(LIBUSB_SOURCE_DIR)" "$(LIBUSB_INSTALL_DIR)" "$(PRIV_DIR)" "$(LIBUSB_CONFIGURE_AC)" ; \
			fi && \
			rm -rf "$(LIBUSB_INSTALL_DIR)" ; \
		fi \
	fi

$(NATIVE_BINDINGS_SO): unarchive_source_code install_libedgetpu_runtime libusb
	@ if [ "$(TFLITE_ELIXIR_ONLY_COPY_PRIV)" = "NO" ]; then \
		if [ ! -e "$(NATIVE_BINDINGS_SO)" ]; then \
			echo "CORAL SUPPORT: $(TFLITE_ELIXIR_CORAL_SUPPORT)" ; \
			echo "LIBEDGETPU runtime: $(TFLITE_ELIXIR_CORAL_LIBEDGETPU_RUNTIME)" ; \
			echo "Throttle USB Coral Devices: $(TFLITE_ELIXIR_CORAL_USB_THROTTLE)" ; \
			git submodule update --init 3rd_party/gflags && \
			git submodule update --init 3rd_party/glog && \
			mkdir -p $(CMAKE_BINDINGS_BUILD_DIR) && \
			python3 "$(shell pwd)/patches/apply_patch.py" "$(TFLITE_ROOT_DIR)" "$(TFLITE_VER)" && \
			cd "$(CMAKE_BINDINGS_BUILD_DIR)" && \
			cmake -D C_SRC="$(C_SRC)" \
			  -D PRIV_DIR="$(PRIV_DIR)" \
			  -D ERTS_INCLUDE_DIR="$(ERTS_INCLUDE_DIR)" \
			  -D TFLITE_ROOT_DIR="$(TFLITE_ROOT_DIR)" \
			  -D GFLAGS_ROOT_DIR="$(GFLAGS_ROOT_DIR)" \
			  -D GLOG_ROOT_DIR="$(GLOG_ROOT_DIR)" \
			  -D TFLITE_ELIXIR_CACHE_DIR="$(TFLITE_ELIXIR_CACHE_DIR)" \
			  -D TFLITE_ELIXIR_CORAL_SUPPORT="$(TFLITE_ELIXIR_CORAL_SUPPORT)" \
			  -D LIBUSB_INSTALL_DIR="$(LIBUSB_INSTALL_DIR)" \
			  $(CMAKE_OPTIONS) \
			  "$(shell pwd)" && \
			make "$(MAKE_BUILD_FLAGS)" && \
			mkdir -p "$(PRIV_DIR)" && \
			cp "$(CMAKE_BINDINGS_BUILD_DIR)/tflite_elixir.so" "$(NATIVE_BINDINGS_SO)" ; \
		fi ; \
	else \
		if [ ! -e "$(NATIVE_BINDINGS_SO)" ]; then \
			rm -rf "$(PRIV_DIR)" ; \
			cp -rf "$(TFLITE_ELIXIR_ONLY_COPY_PRIV)" "$(PRIV_DIR)" ; \
		fi \
	fi

fix_libusb:
	@ bash scripts/macos_libusb_rpath.sh "$(NATIVE_BINDINGS_SO)"
