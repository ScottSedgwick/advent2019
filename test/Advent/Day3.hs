module Advent.Day3 where

import Test.HUnit
import Lib

tests :: Test
tests = TestList
  [ 3 ~=? add 1 2
  ]