#!/usr/bin/env bash

set -euo pipefail

pushd lesson1
./build.sh
popd

pushd lesson2
./build.sh
popd

pushd lesson3
./test.sh
popd

pushd lesson4
./build.sh
popd

pushd lesson5
./build.sh
popd
