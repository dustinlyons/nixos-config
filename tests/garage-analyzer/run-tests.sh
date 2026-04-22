#!/usr/bin/env bash
# Run the garage analyzer test suite.
# Usage: ./tests/garage-analyzer/run-tests.sh
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$REPO_ROOT"

nix shell --impure \
  --expr '(import (builtins.getFlake "nixpkgs") { system = "x86_64-linux"; }).python3.withPackages(ps: with ps; [ pillow numpy opencv4 ])' \
  --command python3 -m unittest -v tests/garage-analyzer/test_analyze.py "$@"
