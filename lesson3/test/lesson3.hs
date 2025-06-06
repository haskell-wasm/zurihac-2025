module Main
  ( main,
  )
where

import Test.QuickCheck

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

main :: IO ()
main = quickCheck prop_reverse
