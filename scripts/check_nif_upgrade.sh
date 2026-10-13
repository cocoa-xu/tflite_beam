#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"
UPGRADE_DIR="$(mktemp -d "${TMPDIR:-/tmp}/tflite-beam-upgrade.XXXXXX")"
trap 'rm -rf "$UPGRADE_DIR"' EXIT

cp priv/tflite_beam.so "$UPGRADE_DIR/old.so"
cp priv/tflite_beam.so "$UPGRADE_DIR/new.so"
ln -s "$ROOT/priv/libedgetpu" "$UPGRADE_DIR/libedgetpu"

python3 - "$UPGRADE_DIR" <<'PY'
from pathlib import Path
import sys

destination = Path(sys.argv[1])
source = Path("src/tflite_beam/tflite_beam_nif.erl").read_text()
start = source.index("init() ->")
end = source.index("\nnot_loaded(", start)
for generation in range(4):
    library = "old" if generation == 0 else "new"
    init = f'''init() ->
    {{ok, [[Dir]]}} = init:get_argument(nif_upgrade_dir),
    erlang:load_nif(filename:join(Dir, "{library}"), 0).

upgrade_generation() -> {generation}.
'''
    directory = destination / f"v{generation}"
    directory.mkdir()
    (directory / "tflite_beam_nif.erl").write_text(source[:start] + init + source[end:])
PY

for version in "$UPGRADE_DIR"/v*; do
    erlc -Werror -o "$version" "$version/tflite_beam_nif.erl"
done
erlc -Werror -o "$UPGRADE_DIR" scripts/upgrade_drive.erl test/tflite_beam_test_models.erl
TFLITE_BEAM_ENABLE_FAULT_INJECTION=1 erl -noshell \
    -pa _build/default/lib/tflite_beam/ebin "$UPGRADE_DIR" \
    -nif_upgrade_dir "$UPGRADE_DIR" -run upgrade_drive main
