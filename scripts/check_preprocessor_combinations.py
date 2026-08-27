#!/usr/bin/env python3
"""Syntax-check every source we compile under each combination of the two
compile-time switches.

Two of the seven precompiled targets build with XNNPACK off, and Coral support
is off for anyone who asks for it -- neither shape is covered by the test
workflows, so a symbol defined inside one `#ifdef` and used outside it compiles
here and fails there, at tag time. This catches that in seconds.

Run it after touching anything under an `#ifdef`:

    python3 scripts/check_preprocessor_combinations.py
"""
import glob
import pathlib
import re
import subprocess
import sys

FLAGS_MAKE = "_build/default/lib/tflite_beam/cmake_tflite_beam/CMakeFiles/tflite_beam.dir/flags.make"


def cmake_var(text, name):
    found = re.search(rf"^{name} = (.*)$", text, re.M)
    return found.group(1) if found else ""


def main():
    flags_make = pathlib.Path(FLAGS_MAKE)
    if not flags_make.is_file():
        print(f"no {FLAGS_MAKE}; build once from source first")
        return 1

    text = flags_make.read_text()
    base_defines, includes = cmake_var(text, "CXX_DEFINES"), cmake_var(text, "CXX_INCLUDES")
    base_flags = cmake_var(text, "CXX_FLAGS")

    # The flags are lifted from a build that already happened, so they describe
    # whatever tree that build used. One from before the runtime moved to LiteRT
    # resolves none of the includes here, and the failure reads as "c_api.h file
    # not found" rather than as a stale build directory.
    if "/litert/" not in includes:
        print(
            f"{FLAGS_MAKE} was written by a build that did not use LiteRT.\n"
            "Rebuild from source in this tree before checking:\n"
            "  rm -f priv/tflite_beam.so && TFLITE_BEAM_PREFER_PRECOMPILED=false make"
        )
        return 1

    sources = sorted(
        glob.glob("c_src/*.cpp")
        + glob.glob("c_src/elitte/**/*.cpp", recursive=True)
    )
    # CMakeLists only compiles this one when Coral support is on
    coral_only = sorted(glob.glob("c_src/coral/*.cpp"))

    failed = False
    for coral in (True, False):
        for xnnpack in (True, False):
            # The LiteRT API is off by default, so that is the shape a plain
            # source build produces and the one a defect in it reaches first.
            # It was missing, and a declaration gated on it whose function
            # table entry was not gated compiled in all four other shapes
            # and broke the default build.
            for litert in (True, False):
                flags = base_flags
                if not coral:
                    flags = flags.replace("-DCORAL_SUPPORT_ENABLED=1", "")
                if not xnnpack:
                    flags = flags.replace("-DTFLITE_BEAM_XNNPACK_ENABLED=1", "")

                # CORAL and XNNPACK live in CXX_FLAGS, the LiteRT one lives in
                # CXX_DEFINES. Editing only flags for all three left every row
                # using whatever the seed build happened to be, so the four
                # LiteRT-on rows and the four off rows were the same four checks
                # printed twice under different labels.
                row_defines = base_defines
                if litert:
                    if "-DTFLITE_BEAM_LITERT_API_ENABLED=1" not in row_defines:
                        row_defines += " -DTFLITE_BEAM_LITERT_API_ENABLED=1"
                else:
                    row_defines = row_defines.replace("-DTFLITE_BEAM_LITERT_API_ENABLED=1", "")

                checking = sources + (coral_only if coral else [])
                label = (f"CORAL={'on ' if coral else 'off'} "
                         f"XNNPACK={'on ' if xnnpack else 'off'} "
                         f"LITERT={'on ' if litert else 'off'}")
                bad = []
                for source in checking:
                    result = subprocess.run(
                        f"c++ {row_defines} {includes} {flags} -fsyntax-only {source}",
                        shell=True, capture_output=True, text=True,
                    )
                    if result.returncode != 0:
                        error = next((l for l in result.stderr.splitlines() if " error: " in l), "(see stderr)")
                        bad.append((source, error.strip()))

                print(f"{label}: {len(checking) - len(bad)}/{len(checking)} compile")
                for source, error in bad:
                    print(f"    {source}: {error[:150]}")
                    failed = True

    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
