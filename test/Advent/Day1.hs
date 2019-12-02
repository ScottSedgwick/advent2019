module Advent.Day1 where

import Test.HUnit
import Lib

tests :: Test
tests = TestList
  [ 2 ~=? fuel1 12
  , 2 ~=? fuel1 14
  , 654 ~=? fuel1 1969
  , 33583 ~=? fuel1 100756

  , 966 ~=? fuel2 1969
  , 50346 ~=? fuel2 100756
  ]