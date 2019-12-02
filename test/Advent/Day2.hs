module Advent.Day2 where

import Test.HUnit
import Lib

tests :: Test
tests = TestList
  [ [3500,9,10,70,2,3,11,0,99,30,40,50] ~=? dec (opcode (enc [1,9,10,3,2,3,11,0,99,30,40,50]))
  , [2,0,0,0,99] ~=? dec (opcode (enc [1,0,0,0,99]))
  , [2,3,0,6,99] ~=? dec (opcode (enc [2,3,0,3,99]))
  , [2,4,4,5,99,9801] ~=? dec (opcode (enc [2,4,4,5,99,0]))
  , [30,1,1,4,2,5,6,0,99] ~=? dec (opcode (enc [1,1,1,4,99,5,6,0,99]))
  ]