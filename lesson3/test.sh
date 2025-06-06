#!/usr/bin/env bash

set -euo pipefail

echo main | wasm32-wasi-cabal repl lesson3-js -v0
