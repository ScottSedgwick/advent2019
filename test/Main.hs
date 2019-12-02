module Main where

import Test.HUnit
import Advent.Day3

main :: IO ()
main = do
  counts <- runTestTT tests
  pure ()