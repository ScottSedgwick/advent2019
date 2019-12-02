module Main where

import Lib

main :: IO ()
main = do
  print $ head $ opcode input1                      -- 2692315
  print $ a * 100 + b                               -- 9507
  where
    xs = [(x,y) | x <- [0..99], y <- [0..99]]
    (a,b) = seek xs
