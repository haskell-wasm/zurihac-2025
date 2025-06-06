# Zurihac 2025 wasm track material

## Building the slides

The slides are available on [GitHub
pages](https://haskell-wasm.github.io/zurihac-2025). To build from
source, run `build.sh` in `slides`. The slides will be available in
`dist/index.html`. Requires `pandoc`.

## Setting up development environment

This repo is a nix flake that provides a default dev shell with all
relevant build tools. Run `nix develop` to enter the shell, and run
`wasm32-wasi-cabal update` at least once in the shell before running
other builds, since `wasm32-wasi-cabal` uses an isolated cabal store
from the default native cabal store to avoid potential interference.

Non-nix users can follow instructions
[here](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta#getting-started-without-nix).

## Building each lesson

The wasm track is organized into five lessons. Each lesson's directory
is a cabal project directory containing a `README.md` with short
description, and a `build.sh` to build it. All build scripts are meant
to be run in that lesson's subdirectory, different lessons don't share
any code.

After building a lesson, run `python3 -m http.server -b localhost
8000` in the lesson's subdirectory to serve a local HTTP server that
loads `index.html`. All `index.html` is self-contained with all
required CSS/JS code.

The repo root `build.sh` builds all lessons at once.

## License

Everything here is licensed under BSD-3.
