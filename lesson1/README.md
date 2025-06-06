# `lesson1`

In `lesson1`, we build our first wasm app:
[`pointfree`](https://hackage.haskell.org/package/pointfree). It will
be built as a self-contained `wasm32-wasi` module, which can be run by
a non-web engine like [`wasmtime`](https://wasmtime.dev) or in the
browser.

Topics covered:

- Using `wasm32-wasi-cabal` to build a wasm module
- Running a self-contained wasm module
- Disassembling a wasm module
- Optimizing a wasm module

To build `pointfree`, run `build.sh` which is equivalent to:

```bash
wasm32-wasi-cabal install --installdir=dist --install-method=copy --overwrite-policy=always pointfree-1.1.1.12
```

To disassemble the wasm module to a readable text format:

```bash
wasm-tools print dist/pointfree.wasm -o dist/pointfree.wat
```

To run it with `wasmtime`:

```bash
wasmtime run -- dist/pointfree.wasm "\(x, y) -> y x"
```

Here's a stock `wasm-opt` command to optimize a wasm module. Add
`--converge` to make it run multiple passes until the wasm module size
no longer shrinks. Specify `BINARYEN_CORES=4` environment variable to
restrict CPU cores.

```bash
$ wasm-opt --low-memory-unused --strip-dwarf -O4 -Oz dist/pointfree.wasm -o dist/pointfree.wasm
$ ls -Alh dist
total 3.6M
-rwxr-xr-x 1 terrorjack users 3.6M Jun  6 11:01 pointfree.wasm
```

To estimate an optimized wasm module's network transmission size:

```bash
$ brotli --best dist/pointfree.wasm

$ ls -Alh dist
total 4.3M
-rwxr-xr-x 1 terrorjack users 3.6M Jun  6 11:01 pointfree.wasm
-rwxr-xr-x 1 terrorjack users 768K Jun  6 11:01 pointfree.wasm.br
```

Comparison against native executable size:

```bash
$ cabal install --installdir=. --install-method=copy --overwrite-policy=always pointfree-1.1.1.12 --allow-newer=all:base --enable-split-sections
$ ls -Alh
total 14M
-rwxr-xr-x 1 runner docker 14M Jun  6 09:02 pointfree
```
