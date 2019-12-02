module Advent.Day2 where

opcode :: [Int] -> [Int]
opcode = process 0

process :: Int -> [Int] -> [Int]
process x xs = if stop then xs' else process (x + 4) xs'
  where
    (xs', stop) = checkOp (take 4 (drop x xs)) xs

checkOp :: [Int] -> [Int] -> ([Int], Bool)
checkOp (99:_) xs = (xs, True)
checkOp [1,in1,in2,out] xs = (op (+) in1 in2 out xs, False)
checkOp [2,in1,in2,out] xs = (op (*) in1 in2 out xs, False)
checkOp _ xs = (xs, True)

op :: (Int -> Int -> Int) -> Int -> Int -> Int -> [Int] -> [Int]
op o in1 in2 out xs = xs'  
  where
    i1 = xs !! in1
    i2 = xs !! in2
    xs' = take out xs ++ [o i1 i2] ++ drop (out + 1) xs

input1 :: [Int]
input1 = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,19,5,23,2,13,23,27,1,10,27,31,2,6,31,35,1,9,35,39,2,10,39,43,1,43,9,47,1,47,9,51,2,10,51,55,1,55,9,59,1,59,5,63,1,63,6,67,2,6,67,71,2,10,71,75,1,75,5,79,1,9,79,83,2,83,10,87,1,87,6,91,1,13,91,95,2,10,95,99,1,99,6,103,2,13,103,107,1,107,2,111,1,111,9,0,99,2,14,0,0]

input2 :: [Int]
input2 = [3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,19,5,23,2,13,23,27,1,10,27,31,2,6,31,35,1,9,35,39,2,10,39,43,1,43,9,47,1,47,9,51,2,10,51,55,1,55,9,59,1,59,5,63,1,63,6,67,2,6,67,71,2,10,71,75,1,75,5,79,1,9,79,83,2,83,10,87,1,87,6,91,1,13,91,95,2,10,95,99,1,99,6,103,2,13,103,107,1,107,2,111,1,111,9,0,99,2,14,0,0]

inp :: (Int, Int) -> [Int]
inp (noun, verb) = 1 : noun : verb : input2

seek :: [(Int, Int)] -> (Int, Int)
seek [] = (-1, -1)
seek (x:xs) =
  if head (opcode (inp x)) == 19690720
  then x
  else seek xs