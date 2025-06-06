module Lesson4
  ( main,
  )
where

import Control.Concurrent.Async
import Control.Monad
import qualified Crypto.Hash.SHA256 as Crypto
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Foldable
import Foreign.Ptr
import GHC.Wasm.Prim
import Test.QuickCheck.Instances ()
import Test.Tasty hiding (defaultMain)
import Test.Tasty.Bench
import Test.Tasty.QuickCheck

foreign export javascript "hsMain"
  main :: IO ()

main :: IO ()
main = do
  let batch_size = 64
      buf_size = 512
  defaultMain $
    map
      (localOption WallTime . localOption (RelStDev 0.2))
      [
        -- Test our sha256 backed by fetch() to a service worker
        -- against cryptohash-sha256
        testProperty "Crypto.hash == hashViaFetch" $ \buf -> ioProperty $ do
          ua <- hashToJSUint8Array buf
          h <- fromJSUint8Array ua
          pure $ Crypto.hash buf == h,

        -- Serially fetch and hash for batch_size times
        bench "serial fetch" $ whnfIO $ replicateM_ batch_size $ do
          buf <- BS.create buf_size $ \_ -> pure ()
          ua <- hashToJSUint8Array buf
          fromJSUint8Array ua,

        -- Do fetch/hash concurrently in batch_size threads
        bench "async fetch" $ whnfIO $ replicateConcurrently_ batch_size $ do
          buf <- BS.create buf_size $ \_ -> pure ()
          ua <- hashToJSUint8Array buf
          fromJSUint8Array ua,

        -- Recommended: fetch first, force the thunks later
        bench "lazy fetch" $ whnfIO $ do
          uas <- replicateM batch_size $ do
            buf <- BS.create buf_size $ \_ -> pure ()
            hashToJSUint8Array buf
          traverse_ fromJSUint8Array uas
      ]

hashToJSUint8Array :: BS.ByteString -> IO JSUint8Array
hashToJSUint8Array bs = BS.unsafeUseAsCStringLen bs $ uncurry jsHashViaFetch

fromJSUint8Array :: JSUint8Array -> IO BS.ByteString
fromJSUint8Array ua = BS.create len $ \ptr -> jsMemset ptr len ua
  where
    len = jsByteLength ua

newtype JSUint8Array = JSUint8Array JSVal

-- This calls fetch() eagerly, but returns a thunk that evaluates to
-- the Promise fulfilled result, only when forced later. It allows
-- concurrency without creating new threads. See
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html#foreign-imports
-- for details.
foreign import javascript safe
  "const resp = await fetch('/sha256', {method: 'POST', body: new Uint8Array(__exports.memory.buffer, $1, $2)}); return resp.bytes();"
  jsHashViaFetch :: Ptr a -> Int -> IO JSUint8Array

foreign import javascript unsafe
  "$1.byteLength"
  jsByteLength :: JSUint8Array -> Int

foreign import javascript unsafe
  "(new Uint8Array(__exports.memory.buffer, $1, $2)).set($3)"
  jsMemset :: Ptr a -> Int -> JSUint8Array -> IO ()
