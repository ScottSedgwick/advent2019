module Main where

import Test.HUnit
import Advent.Day2

main :: IO ()
main = do
  counts <- runTestTT tests
  pure ()