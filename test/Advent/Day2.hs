module Advent.Day2 where

import Test.HUnit
import Lib

tests :: Test
tests = TestList
  [ 2 ~=? add 1 1
  ]