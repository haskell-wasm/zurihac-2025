---
author: Cheng Shao
title: Ingredients for cooking Haskell wasm apps
date: June 7, 2025
---

## About the wasm track

- <https://github.com/haskell-wasm/zurihac-2025>
- 5 lessons, each with a small demo
  - Complement the GHC user [manual](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
  - For more internal details, checkout my HIW'23 [talk](https://www.youtube.com/watch?v=ujkmV1DJKv4)
- Focus on raw ingredients
  - [`miso`](https://haskell-miso.org), [`reflex`](https://reflex-frp.org), [`jsaddle`](https://hackage.haskell.org/package/jsaddle) etc are beyond this talk's scope
- For live questions, please ask in discord #wasm channel and tag me

## Lesson 1: your first wasm module

- Let's build the [`pointfree`](https://hackage.haskell.org/package/pointfree) CLI
- Use `wasm32-wasi-cabal` to build the executable component

## What is a wasm module

- Write once run anywhere, hopefully done right this time
- A quick look at disassembled module
- [Key concepts](https://webassembly.github.io/spec/core/intro/overview.html#concepts)
  - Linear memory
  - Import/export functions
  - [WASI](https://wasi.dev)
  - Module/instance

## Running a wasm module

- Use [`wasmtime`](https://wasmtime.dev)
  - Only works for self-contained `wasm32-wasi` module
- In a browser
  - Or other JavaScript hosts: node.js/deno, [Cloudflare](https://developers.cloudflare.com/workers/runtime-apis/webassembly)/[Fastly](https://docs.fastly.com/products/compute) etc

## Running `pointfree.wasm` in a browser

- Use [`@bjorn3/browser_wasi_shim`](https://www.jsdelivr.com/package/npm/@bjorn3/browser_wasi_shim) for WASI imports
- [Compiling](https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/JavaScript_interface/compileStreaming_static) a module
- [Instantiating](https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/JavaScript_interface/instantiate_static) a module
- Starting the CLI
- Using virtual stdio & filesystem

## Optimizing `pointfree.wasm`

- Use [`wasm-opt`](https://github.com/WebAssembly/binaryen) to optimize `pointfree.wasm`
- Use `brotli` to estimate transferred wasm module size
- Comparing size against native
  - Unoptimized `pointfree.wasm`: `8.5M`
  - Optimized `pointfree.wasm`: `3.6M`
  - x86_64-linux `pointfree` with split sections: `14M`

## Lesson 2: basic JSFFI

- Let's port [`hledger-lib`](https://hackage.haskell.org/package/hledger-lib) to the browser
- Need to build the wasm module & a companion JS module

## A closer look at demo code

- `foreign import javascript unsafe`
- `foreign export javascript`
- Special link-time GHC options

## Basic foreign types in JSFFI

- Boxed foreign types: `Int`, `Ptr`, etc
- `JSVal` in `GHC.Wasm.Prim`
  - Can be any JS value
  - Representation: unique key in a mapping + weak pointer
  - Garbage collected, can be eagerly freed via `freeJSVal`
- `JSString` and other `newtype`s of `JSVal`

## `foreign import javascript unsafe`

- Call a synchronous JS function
- Source snippet: JS expression or statements (with `return` to return the result)
- Use `$1`, `$2` etc for Haskell function arguments
- Arguments & result value are fully evaluated

## `foreign export javascript`

- Default `main` doesn't work, at least one export is required as entry point
- Synchronous export
  - Prevent interleaved execution, e.g. [`requestAnimationFrame`](https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame) callback
- Async export (default)
  - Enable calling async JS function, e.g. [`fetch`](https://developer.mozilla.org/en-US/docs/Web/API/Window/fetch)

## Using ghci & ghcid

- `wasm32-wasi-cabal repl`
  - [`--repl-options`](https://cabal.readthedocs.io/en/latest/cabal-commands.html#cmdoption-repl-options) to pass ghc options
  - [`--enable-multi-repl`](https://cabal.readthedocs.io/en/latest/cabal-commands.html#cmdoption-enable-multi-repl) to load multiple components at once
- ghci browser mode
  - [`--repl-options=-fghci-browser`](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#ghc-flag-fghci-browser)
  - Works with Firefox/Chrome based browsers
  - More details in my [blog post](https://www.tweag.io/blog/2025-04-17-wasm-ghci-browser)

## Lesson 3: get your cabal project tested

- [`tasty`](https://hackage.haskell.org/package/tasty), [`QuickCheck`](https://hackage.haskell.org/package/tasty-quickcheck), [`HUnit`](https://hackage.haskell.org/package/tasty-hunit) etc work on wasm
- [`wasm32-wasi-cabal test --test-wrapper=wasmtime`](https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-test-test-wrapper)
  - Falls short if JSFFI is used
- Use ghci as a test runner
  - Browser mode also works

## Lesson 4: advanced JSFFI

- [Service Worker](https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API) in the demo
  - Intercepts `fetch` calls as poor man's backend in the frontend
  - `/sha256`: use Web Crypto API to [hash](https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest) the request payload
- Haskell demo
  - Marshaling buffers in JSFFI
  - Import async JS function like `fetch`

## Marshaling buffers

- In JSFFI code, `__exports.memory` is a [`Memory`](https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/JavaScript_interface/Memory) object
  - You can [construct](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray#buffer) a `TypedArray` by wrapping [`__exports.memory.buffer`](https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/JavaScript_interface/Memory/buffer)
- Always use pinned `ByteArray#`
- Pay attention to buffer lifetime
  - Linear memory may grow during GC, previous buffer is [detached](https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/JavaScript_interface/Memory/grow#detachment_upon_growing)
  - Make sure the buffer is fully consumed in JSFFI code
- Exercise: marshaling [`text`](https://hackage.haskell.org/package/aeson), [`aeson`](https://hackage.haskell.org/package/aeson)

## `foreign import javascript safe`

- Call an async JS function that returns a [`Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
  - [`await`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/await) allowed in source snippet
- Returns a thunk immediately
  - When the thunk is forced, blocks current thread until `Promise` is fulfilled
  - Doesn't block other Haskell threads
  - Allows concurrency without threading overhead
- JS exception can be caught as Haskell exception

## Lesson5: a 2D [`canvas`](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D) example

- Draw a simple `canvas` 2D animation
- Use [`JuicyPixels`](https://hackage.haskell.org/package/JuicyPixels) to represent each frame

## `foreign import javascript "wrapper"`

- Dynamically export a Haskell function closure as a JS callback
- Sync/async variants similar to static exports
- See [`FunPtr`](https://hackage.haskell.org/package/base/docs/Foreign-Ptr.html#t:FunPtr) for the same thing in C FFI

## Future work

- In the GHC land
  - Threaded RTS & parallelism ([#25442](https://gitlab.haskell.org/ghc/ghc/-/issues/25442))
  - Faster ghci ([#25407](https://gitlab.haskell.org/ghc/ghc/-/issues/25407))
  - Eventlog & profiling in browser
- In the user land
  - Official library for efficiently marshaling `text`, `aeson`, etc
  - More user-friendly post-linker
  - TypeScript bindgen
- Tell me your pain spots & wishlist!

## More resources

- [`#haskell-wasm:matrix.org`](https://matrix.to/#/#haskell-wasm:matrix.org)
- [`ghc-wasm-meta`](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta)
- GHC user [manual](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html)
- Example repo for [`miso`](https://github.com/tweag/ghc-wasm-miso-examples) and [`reflex`](https://github.com/tweag/ghc-wasm-reflex-examples)
- [`jsaddle-wasm`](https://hackage.haskell.org/package/jsaddle-wasm)

## Q/A
