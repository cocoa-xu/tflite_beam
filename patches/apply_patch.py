#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
from pathlib import Path
if sys.version_info[0] >= 3:
    from io import StringIO
else:
    from cStringIO import StringIO


def patch_fix_RoundToNearest(tf_version: str, tf_src_root: str):
    if tf_version not in ['2.11.0', '2.11.1']:
        print(f"warning: skip applying `patch_fix_RoundToNearest` to tf version `{tf_version}`")
        return

    # kernels/internal/optimized/neon_tensor_utils.cc
    neon_tensor_utils_cc = Path(tf_src_root) / 'kernels' / 'internal' / 'optimized' / 'neon_tensor_utils.cc'
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

patches = [patch_fix_RoundToNearest]


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
