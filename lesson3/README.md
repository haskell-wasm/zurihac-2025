# `lesson3`

`lesson3` is a simple cabal project with two test components,
`lesson3` (doesn't use JSFFI) and `lesson3-js` (uses JSFFI).

Topics covered:

- Testing a wasm cabal project.
- Running stuff in ghci browser mode.

To test `lesson3`:

```bash
wasm32-wasi-cabal test lesson3 --test-wrapper=wasmtime
```

To test `lesson3-js`, run `test.sh` which is equivalent to:

```bash
echo main | wasm32-wasi-cabal repl lesson3-js -v0
```

The `-v0` flag suppresses noise. Note that ghci doesn't exit with
non-zero code even if `main` throws, so in CI you should `grep` for
output and check for expected line like `OK, passed 100 tests.`

It's even possible to use ghci browser mode to run a cabal test in a
browser automatically, see relevant GHC
[flags](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#ghc-flag-fghci-browser-puppeteer-launch-opts)
for details.
