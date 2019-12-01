module Advent.Day1 where

import Test.HUnit
import Lib

tests :: Test
tests = TestList
  [ 2 ~=? fuel 12
  , 2 ~=? fuel 14
  , 654 ~=? fuel 1969
  , 33583 ~=? fuel 100756

  , 966 ~=? ffuels 1969
  , 50346 ~=? ffuels 100756
  ]