module Main where

import Test.HUnit (Counts(..), runTestTT)
import System.Exit (die)
import Advent.Day3

main :: IO ()
main = do
  counts <- runTestTT tests
  if (errors counts > 0) || (failures counts > 0)
  then die "Failed unit tests."
  else pure ()
