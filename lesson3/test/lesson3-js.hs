module Main
  ( main,
  )
where

import GHC.Wasm.Prim
import Test.QuickCheck

main :: IO ()
main = quickCheck $ \s -> s == fromJSString (toJSString s)
