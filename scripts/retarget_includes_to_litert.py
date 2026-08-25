#!/usr/bin/env python3
"""Rewrite `tensorflow/lite/...` includes in a vendored tree to `tflite/...`.

LiteRT keeps the runtime under `tflite/` and ships no `tensorflow/` directory at
all, so a vendored dependency that still asks for `tensorflow/lite/interpreter.h`
does not fail to build: it silently finds TensorFlow's own copy, which is on the
include path because LiteRT's CMake needs it for other sources. The result links
one definition of a class against a library built from another. It builds, most
of it runs, and the parts that touch the differing members read the wrong memory.

Rewriting is therefore not cosmetic. It is what makes the include unambiguous.

Idempotent: a tree that has already been rewritten is left alone.
"""

import sys
from pathlib import Path

SUFFIXES = {".c", ".cc", ".cpp", ".h", ".hpp", ".inc"}


def retarget(root: Path) -> int:
    changed = 0
    for path in root.rglob("*"):
        if not path.is_file() or path.suffix not in SUFFIXES:
            continue
        try:
            text = path.read_text()
        except (UnicodeDecodeError, OSError):
            continue
        rewritten = text.replace('"tensorflow/lite/', '"tflite/').replace(
            "<tensorflow/lite/", "<tflite/"
        )
        if rewritten == text:
            # prose mentioning the old path is not an include and is left alone,
            # which is also what makes a second run report nothing to do
            continue
        path.write_text(rewritten)
        changed += 1
    return changed


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: retarget_includes_to_litert.py <tree>", file=sys.stderr)
        return 2
    root = Path(sys.argv[1])
    if not root.is_dir():
        print(f"{root} is not a directory", file=sys.stderr)
        return 1
    print(f"retargeted {retarget(root)} files under {root} to the tflite prefix")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
