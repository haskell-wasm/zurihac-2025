# `lesson2`

`lesson2` is a simple frontend app that parses a plain-text ledger
using
[`hledger-lib`](https://hackage.haskell.org/package/hledger-lib), and
renders it to HTML in Haskell.

Unlike `lesson1`, `lesson2` and future lessons make use of the
[JSFFI](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#javascript-ffi-in-the-wasm-backend)
feature, therefore it can only be run in the browser.

Topics covered:

- Building a wasm frontend app that uses JSFFI.
- Basic JSFFI: marshaling basic types; synchronous import & export.
- Using `ghci`/`ghcid` in a wasm cabal project.
