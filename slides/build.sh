#!/usr/bin/env bash

set -euxo pipefail

mkdir -p dist

pandoc \
  -t revealjs \
  -i \
  -V revealjs-url=https://cdn.jsdelivr.net/npm/reveal.js@5.2.1 \
  -V slideNumber=true \
  -V theme=simple \
  -s \
  index.md \
  -o dist/index.html
