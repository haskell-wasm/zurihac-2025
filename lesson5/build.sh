#!/usr/bin/env bash

set -euxo pipefail

wasm32-wasi-cabal install --installdir=dist --install-method=copy --overwrite-policy=always

$(wasm32-wasi-ghc --print-libdir)/post-link.mjs -i dist/lesson5.wasm -o dist/ghc_wasm_jsffi.js
