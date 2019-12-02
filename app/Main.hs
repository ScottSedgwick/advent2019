module Main where

import Lib
import Data.IntMap

main :: IO ()
main = do
  print $ opcode input1 ! 0                         -- 2692315
  print $ a * 100 + b                               -- 9507
  where
    xs = [(x,y) | x <- [0..99], y <- [0..99]]
    (a,b) = seek xs
