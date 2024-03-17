#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
from pathlib import Path
if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO


def patch_fix_RoundToNearest(tf_version: str, tf_src_root: str):
    if tf_version not in ['2.11.0', '2.11.1', '2.12.0']:
        print(f"warning: skip applying `patch_fix_RoundToNearest` to tf version `{tf_version}`")
        return

    # tensorflow/lite/kernels/internal/optimized/neon_tensor_utils.cc
    tf_lite_src_root = Path(tf_src_root) / 'tensorflow' / 'lite'
    neon_tensor_utils_cc = Path(tf_lite_src_root) / 'kernels' / 'internal' / 'optimized' / 'neon_tensor_utils.cc'
    fixed = StringIO()
    patched_1 = False
    skip_next_line = False
    with open(neon_tensor_utils_cc, 'r') as source:
        for line in source:
            if skip_next_line:
                skip_next_line = False
                continue
            line_strip = line.strip()
            if not patched_1 and line_strip == 'inline int32x4_t RoundToNearest(const float32x4_t input) {':
                fixed.write(f"{line_strip}\n")
                fixed.write(f"#if (__ARM_ARCH >= 8) && (defined(TF_HAS_vcvtaq_s32_f32))\n")
                skip_next_line = True
                patched_1 = True
            else:
                fixed.write(line)

    if patched_1:
        with open(neon_tensor_utils_cc, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())

def patch_compiling_telemetry_cc(tf_version: str, tf_src_root: str):
    if tf_version not in ['2.12.0']:
        print(f"warning: skip applying `patch_compiling_telemetry_cc` to tf version `{tf_version}`")
        return
    
    tf_lite_src_root = Path(tf_src_root) / 'tensorflow' / 'lite'
    cmakelists_txt = Path(tf_lite_src_root) / 'CMakeLists.txt'
    fixed = StringIO()
    patched_1 = False
    with open(cmakelists_txt, 'r') as source:
        for line in source:
            line_strip = line.strip()
            if not patched_1 and line_strip == '${TFLITE_SOURCE_DIR}/profiling/telemetry/profiler.cc':
                fixed.write(f"  {line_strip} # fixed\n")
                fixed.write("  ${TFLITE_SOURCE_DIR}/profiling/telemetry/telemetry.cc\n")
                patched_1 = True
            else:
                fixed.write(line)

    if patched_1:
        with open(cmakelists_txt, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())


def patch_cpuinfo_riscv64_sys_hwprobe(tf_version: str, tf_src_root: str):
    if tf_version not in ['2.16.0', '2.16.1']:
        print(f"warning: skip applying `patch_cpuinfo_riscv64_sys_hwprobe` to tf version `{tf_version}`")
        return
    
    # tensorflow/lite/tools/cmake/modules/cpuinfo.cmake
    # tensorflow/workspace2.bzl
    tf_lite_src_root = Path(tf_src_root) / 'tensorflow' / 'lite'
    cpuinfo_cmake = Path(tf_lite_src_root) / 'tools' / 'cmake' / 'modules' / 'cpuinfo.cmake'
    fixed = StringIO()
    patched_1 = False
    lines_to_skip = 0
    with open(cpuinfo_cmake, 'r') as source:
        for line in source:
            line_strip = line.strip()
            if not patched_1 and line_strip == '# Sync with tensorflow/third_party/cpuinfo/workspace.bzl':
                fixed.write(f"  {line_strip} # fixed\n")
                fixed.write("  GIT_TAG 6543fec09b2f04ac4a666882998b534afc9c1349\n")
                patched_1 = True
                lines_to_skip = 1
            else:
                if lines_to_skip > 0:
                    lines_to_skip -= 1
                    continue
                fixed.write(line)

    if patched_1:
        with open(cpuinfo_cmake, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())

    workspace2_bzl = Path(tf_src_root) / 'tensorflow' / 'workspace2.bzl'
    fixed = StringIO()
    patched_2 = False
    with open(workspace2_bzl, 'r') as source:
        for line in source:
            line_strip = line.strip()
            if not patched_2 and line_strip == 'name = "cpuinfo",':
                fixed.write('        name = "cpuinfo", # fixed\n')
                fixed.write('        strip_prefix = "cpuinfo-6543fec09b2f04ac4a666882998b534afc9c1349",\n')
                fixed.write('        sha256 = "17180581df58b811ef93cfafd074598966a185f48e5a574e8947ca51419f7ca6",\n')
                fixed.write('        urls = tf_mirror_urls("https://github.com/pytorch/cpuinfo/archive/6543fec09b2f04ac4a666882998b534afc9c1349.zip"),\n')
                patched_2 = True
                lines_to_skip = 3
            else:
                if lines_to_skip > 0:
                    lines_to_skip -= 1
                    continue
                fixed.write(line)

    if patched_2:
        with open(workspace2_bzl, 'w') as dst:
            dst.truncate(0)
            dst.write(fixed.getvalue())



patches = [patch_fix_RoundToNearest, patch_compiling_telemetry_cc, patch_cpuinfo_riscv64_sys_hwprobe]

if __name__ == '__main__':
    tf_version = None
    tf_src_root = None
    if len(sys.argv) != 3:
        sys.exit(1)
    tf_src_root = sys.argv[1]
    tf_version = sys.argv[2]
    print(f"[+] applying patches to Tensorflow {tf_version} at {tf_src_root}")
    for p in patches:
        p(tf_version, tf_src_root)
