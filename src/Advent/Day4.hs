module Advent.Day4 where

import Data.List (any, group)

validPass1 :: Int  -> Bool
validPass1 x = monotonicUp s && length g < length s
  where
    s = show x
    g = group s

validPass2 :: Int  -> Bool
validPass2 x = validPass1 x && any (\y -> length y == 2) g
  where
    s = show x
    g = group s

monotonicUp :: Ord a => [a] -> Bool
monotonicUp (x:y:zs) = (x <= y) && monotonicUp (y:zs)
monotonicUp _ = True
