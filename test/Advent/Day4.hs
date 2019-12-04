module Advent.Day4 where

import Test.HUnit
import Lib

d4tests = 
  [ True  ~=? validPass1 111111
  , False ~=? validPass1 223450
  , False ~=? validPass1 123789
  , True  ~=? validPass2 112233
  , False ~=? validPass2 123444
  , True  ~=? validPass2 111122
  ]

tests :: Test
tests = TestList d4tests