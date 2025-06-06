#!/usr/bin/env bash

set -euxo pipefail

wasm32-wasi-cabal install --installdir=dist --install-method=copy --overwrite-policy=always pointfree-1.1.1.12
