# `lesson4`

`lesson4` is a frontend app with two components:

- A service worker that intercepts `fetch()` requests and implements a
  `/sha256` endpoint using Web Crypto API
- A Haskell module that tests/benchmarks a `sha256` hasher backed by
  `fetch()` requests

Topics covered:

- Advanced JSFFI: marshaling bufferes; async import & export
- How Concurrent Haskell fits in the JS concurrency model
