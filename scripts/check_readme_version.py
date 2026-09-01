#!/usr/bin/env python3
"""The README tells people which version to depend on, and that line went stale:
it still named 1.0.0-rc1 three releases later, so anyone following it got a
build without the fixes the surrounding paragraph describes. Nothing failed,
because nothing was looking."""

import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent

APP_SRC = ROOT / "src" / "tflite_beam.app.src"
README = ROOT / "README.md"


def app_version() -> str:
    match = re.search(r'\{vsn,\s*"([^"]+)"\}', APP_SRC.read_text())
    if not match:
        sys.exit(f"no {{vsn, ...}} in {APP_SRC}")
    return match.group(1)


def readme_versions() -> list[tuple[int, str]]:
    """Every tflite_beam requirement the README spells out in full."""
    found = []
    for number, line in enumerate(README.read_text().splitlines(), 1):
        match = re.search(r'\{tflite_beam,\s*"([^"]+)"\}', line)
        if match:
            found.append((number, match.group(1)))
    return found


def is_exact(requirement: str) -> bool:
    """A range such as ~> 0.3 names a family and does not go stale; an exact
    version names one release and does."""
    return not requirement.strip().startswith(("~>", ">=", "<=", ">", "<", "==")) \
        and requirement.strip() != ""


def main() -> int:
    version = app_version()
    prerelease = "-" in version
    found = readme_versions()
    if not found:
        print("no {tflite_beam, \"...\"} requirement in README.md to check")
        return 0

    exact = [(n, r) for n, r in found if is_exact(r)]
    stale = []
    for number, requirement in exact:
        if requirement == version:
            continue
        # A pre-release has to be named exactly, so the README carries one and it
        # is the one that goes stale. A requirement that is itself a release
        # (1.0.0, 0.3.12) is a deliberate reference to an older version.
        if "-" in requirement or prerelease and requirement.startswith(version.split("-")[0]):
            stale.append((number, requirement))

    print(f"src/tflite_beam.app.src: {version}")
    for number, requirement in found:
        mark = "stale" if (number, requirement) in stale else "ok"
        print(f"  README.md:{number}: {{tflite_beam, \"{requirement}\"}}  [{mark}]")

    if stale:
        print()
        for number, requirement in stale:
            print(f"README.md:{number} tells people to depend on {requirement}, "
                  f"but this is {version}")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
