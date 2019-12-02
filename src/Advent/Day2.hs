module Advent.Day2 where

import Data.IntMap.Lazy

encode :: Int -> [Int] -> IntMap Int
encode x = fromAscList . zip [x..]

enc :: [Int] -> IntMap Int
enc = encode 0

dec :: IntMap Int -> [Int]
dec = elems

opcode :: IntMap Int -> IntMap Int
opcode = opcode' 0

opcode' :: Int -> IntMap Int -> IntMap Int
opcode' i zs = 
  case zs ! i of
    99 -> zs
    1  -> opcode' (i + 4) (insert (zs ! (i + 3)) (zs ! (zs ! (i + 1)) + (zs ! (zs ! (i + 2)))) zs)
    2  -> opcode' (i + 4) (insert (zs ! (i + 3)) (zs ! (zs ! (i + 1)) * (zs ! (zs ! (i + 2)))) zs)
    _  -> zs

op4 :: Int -> (Int -> Int -> Int) -> IntMap Int -> IntMap Int
op4 i op zs = insert c v zs
  where 
    a = zs ! (i + 1)
    b = zs ! (i + 2)
    c = zs ! (i + 3)
    d = zs ! a
    e = zs ! b
    v = op d e

input1 :: IntMap Int
input1 = inp (12, 2)

input2 :: IntMap Int
input2 = insert 0 1$ fromAscList $ zip [3..] [3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,19,5,23,2,13,23,27,1,10,27,31,2,6,31,35,1,9,35,39,2,10,39,43,1,43,9,47,1,47,9,51,2,10,51,55,1,55,9,59,1,59,5,63,1,63,6,67,2,6,67,71,2,10,71,75,1,75,5,79,1,9,79,83,2,83,10,87,1,87,6,91,1,13,91,95,2,10,95,99,1,99,6,103,2,13,103,107,1,107,2,111,1,111,9,0,99,2,14,0,0]

inp :: (Int, Int) -> IntMap Int
inp (noun, verb) = insert 1 noun (insert 2 verb input2)

seek :: [(Int, Int)] -> (Int, Int)
seek [] = (-1, -1)
seek (x:xs) =
  if opcode (inp x) ! 0 == 19690720
  then x
  else seek xs