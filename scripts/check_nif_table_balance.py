#!/usr/bin/env python3
"""Every NIF the Erlang side declares must be registered in every build shape.

The failure this catches is quiet in one direction and loud in the other. A
function the .so registers and the module does not declare makes on_load fail,
so nothing in the library works and the reason names the function. A function
the module declares and the .so does not register raises {not_loaded, ...} on
the day someone calls it, from a build where every neighbouring function gives
a sentence explaining that the LiteRT API was not compiled in.

scripts/check_preprocessor_combinations.py compiles all eight shapes and would
not notice either: both are consistent C++.
"""
import re
import sys
from pathlib import Path

root = Path(__file__).resolve().parent.parent
erl = (root / "src/tflite_beam/tflite_beam_nif.erl").read_text()
declared = {n for n in re.findall(r"^([a-z_0-9]+)\(", erl, re.M)} - {"not_loaded", "init"}

src = (root / "c_src/bindings.cpp").read_text()
table = src[src.index("static ErlNifFunc nif_functions"):]
table = table[:table.index("\n};")]

registered = set(re.findall(r"\bF(?:_CPU|_IO)?\((\w+),", table))
stubbed = set(re.findall(r"\bF_NOT_COMPILED[A-Z_]*\((\w+),", table))

problems = []
for name in sorted(declared):
    where = []
    if name not in registered:
        where.append("the real table")
    if name not in stubbed and name not in registered:
        where.append("the not-compiled table")
    if name not in registered and name not in stubbed:
        problems.append(f"{name}: in neither table, so it can only ever raise")

# Anything inside a conditional needs an entry on both sides of it.
for name in sorted(declared & (registered ^ stubbed)):
    # present in exactly one of the two: fine only if it is unconditional, which
    # means it is not inside any #ifdef in the table
    pattern = re.compile(r"^\s*F(?:_CPU|_IO|_NOT_COMPILED[A-Z_]*)?\(" + re.escape(name) + r",", re.M)
    lines = [table[:m.start()].count("\n") for m in pattern.finditer(table)]
    if not lines:
        continue
    before = table[: table.index("\n", table.find("\n" * 0)) ] if False else None
    depth_at = []
    depth = 0
    for i, line in enumerate(table.split("\n")):
        stripped = line.strip()
        if stripped.startswith("#if"):
            depth += 1
        elif stripped.startswith("#endif"):
            depth -= 1
        depth_at.append(depth)
    for ln in lines:
        if depth_at[ln] > 0:
            problems.append(
                f"{name}: inside a conditional but only in "
                + ("the real table" if name in registered else "the not-compiled table")
                + ", so the other shape leaves it raising {not_loaded, ...}"
            )

extra = sorted((registered | stubbed) - declared)
for name in extra:
    problems.append(f"{name}: registered in C but not declared in tflite_beam_nif.erl, which makes on_load fail")

print(f"{len(declared)} NIFs declared, {len(registered)} registered, {len(stubbed)} stubbed")
if problems:
    print("\nunbalanced:")
    for p in problems:
        print(f"  {p}")
    sys.exit(1)
print("every declared NIF is answerable in every build shape")
