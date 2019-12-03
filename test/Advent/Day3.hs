module Advent.Day3 where

import Data.List (intersect)
import Test.HUnit
import Lib
import qualified Data.IntMap.Strict as M

d3tests = [
    [(Rt, 8), (Up, 5), (Lt, 5), (Dn, 3)] ~=? parse "R8,U5,L5,D3"
  , [(Up, 7), (Rt, 6), (Dn, 4), (Lt, 4)] ~=? parse "U7,R6,D4,L4"
  , M.fromList [(0,0), (10000,1), (20000,2), (30000,3), (40000,4)] ~=? wirepath 0 0 [(Rt, 5)] M.empty
  , M.fromList [(0,0), (60005,30), (30003, 40)] ~=? intersection (wirepath 0 0 (parse "R8,U5,L5,D3") M.empty) (wirepath 0 0 (parse "U7,R6,D4,L4") M.empty)
  ]

d3p1tests = [
    6 ~=? d3p1 "R8,U5,L5,D3" "U7,R6,D4,L4"
  , 135 ~=? d3p1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  , 159 ~=? d3p1 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"
  ]

d3p2tests = [
    30 ~=? d3p2 "R8,U5,L5,D3" "U7,R6,D4,L4"
  , 610 ~=? d3p2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"
  , 410 ~=? d3p2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
  ]

tests :: Test
tests = TestList (d3tests ++ d3p1tests ++ d3p2tests)