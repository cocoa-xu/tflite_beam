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
import shutil
import subprocess
import sys
import tempfile

FLAGS_MAKE = "_build/default/lib/tflite_beam/cmake_tflite_beam/CMakeFiles/tflite_beam.dir/flags.make"
CMAKE_FILES = "_build/default/lib/tflite_beam/cmake_tflite_beam/CMakeFiles"


def cmake_var(text, name):
    found = re.search(rf"^{name} = (.*)$", text, re.M)
    return found.group(1) if found else ""




def compiler_for(flags_make):
    """The compiler the flags were written for, not whatever c++ means here.

    The flags are lifted from the last build in this tree, and that build may
    have been a cross one. Feeding an armv6 or riscv64 include path to the host
    c++ produces rows that pass or fail for reasons that have nothing to do with
    the #ifdef shapes being checked, which is what happened on 2026-08-29 after
    a riscv64 build: four rows went green against riscv64 headers compiled by
    Apple clang, and the coral rows went red because that build had Coral off.
    """
    for cmake_cxx in sorted(pathlib.Path(CMAKE_FILES).glob("*/CMakeCXXCompiler.cmake")):
        found = re.search(r'set\(CMAKE_CXX_COMPILER "([^"]+)"', cmake_cxx.read_text())
        if found and pathlib.Path(found.group(1)).is_file():
            return found.group(1)
    return "c++"


def litert_include_flags():
    """What a LiteRT-on build puts on the include path and a default one does not.

    Returns the -I flags, or None after explaining what is missing. Never
    returns flags that would let a row pass by not reaching LiteRT at all.
    """
    roots = sorted(glob.glob("3rd_party/litert/LiteRT-*"))
    if not roots:
        print("no 3rd_party/litert/LiteRT-*; unpack the LiteRT source before checking")
        return None
    root = pathlib.Path(roots[-1])

    template = root / "litert/build_common/build_config.h.in"
    if not template.is_file():
        print(f"no {template}; the LiteRT source tree is not the shape this expects")
        return None

    # CMake writes this with configure_file; #cmakedefine01 becomes a 0/1 define.
    # GPU and NPU are off here because that is what CMakeLists defaults them to
    # and therefore what the precompiled LiteRT tarballs are built with.
    generated = pathlib.Path(tempfile.mkdtemp(prefix="tflite_beam_litert_")) / "litert/build_common"
    generated.mkdir(parents=True)
    body = template.read_text()
    body = re.sub(r"^#cmakedefine01 (\w+)$", r"#define \1 1", body, flags=re.M)
    (generated / "build_config.h").write_text(body)

    header = generated / "build_config.h"
    if "#define LITERT_BUILD_CONFIG_DISABLE_GPU" not in header.read_text():
        print(f"{header} came out without the defines it exists to carry")
        return None

    return f"-I{generated.parent.parent} -I{root}"


def main():
    flags_make = pathlib.Path(FLAGS_MAKE)
    if not flags_make.is_file():
        print(f"no {FLAGS_MAKE}; build once from source first")
        return 1

    text = flags_make.read_text()
    compiler = compiler_for(flags_make)
    base_defines, includes = cmake_var(text, "CXX_DEFINES"), cmake_var(text, "CXX_INCLUDES")
    base_flags = cmake_var(text, "CXX_FLAGS")

    # The flags are lifted from a build that already happened, so they describe
    # whatever tree that build used. One from before the runtime moved to LiteRT
    # resolves none of the includes here, and the failure reads as "c_api.h file
    # not found" rather than as a stale build directory.
    #
    # Checking for "/litert/" was not enough and looked like it was: the tflite
    # subtree lives under 3rd_party/litert too, so a LiteRT-*off* build passes
    # that test while missing everything the LiteRT rows need. On CI, where no
    # LiteRT build has ever run, all four of those rows failed on a header the
    # off build never generates, and locally they passed on one left behind by
    # an earlier build. Ask for the header the tflite side actually needs.
    if not any(pathlib.Path(d.lstrip("-I")).joinpath("tflite/c/c_api.h").is_file()
               for d in includes.split() if d.startswith("-I")):
        print(
            f"{FLAGS_MAKE} was written by a build that did not use LiteRT.\n"
            "Rebuild from source in this tree before checking:\n"
            "  rm -f priv/tflite_beam.so && TFLITE_BEAM_PREFER_PRECOMPILED=false make"
        )
        return 1

    # A LiteRT-on row needs what a LiteRT-on build would have and a default
    # build does not: LITERT_ROOT_DIR on the include path, and build_config.h,
    # which CMake generates from a template into its own binary directory. If
    # the flags came from a default build neither is there, so they are made
    # here rather than hoped for.
    print(f"checking with {compiler}")
    litert_includes = litert_include_flags()
    if litert_includes is None:
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
                row_includes = includes
                if litert:
                    if "-DTFLITE_BEAM_LITERT_API_ENABLED=1" not in row_defines:
                        row_defines += " -DTFLITE_BEAM_LITERT_API_ENABLED=1"
                    row_includes = f"{includes} {litert_includes}"
                else:
                    row_defines = row_defines.replace("-DTFLITE_BEAM_LITERT_API_ENABLED=1", "")

                checking = sources + (coral_only if coral else [])
                label = (f"CORAL={'on ' if coral else 'off'} "
                         f"XNNPACK={'on ' if xnnpack else 'off'} "
                         f"LITERT={'on ' if litert else 'off'}")
                bad = []
                for source in checking:
                    result = subprocess.run(
                        f"{compiler} {row_defines} {row_includes} {flags} -fsyntax-only {source}",
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
