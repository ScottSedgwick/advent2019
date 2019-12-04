module Main where

import Lib

input :: [Int]
input = [248345..746315]

main :: IO ()
main = do
  print $ length $ filter validPass1 input  -- 1019
  print $ length $ filter validPass2 input  -- 660