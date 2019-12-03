module IntCode (enc, dec, opcode) where

import Data.IntMap.Lazy

encode :: Int -> [Int] -> IntMap Int
encode x = fromAscList . zip [x..]

enc :: [Int] -> IntMap Int
enc = encode 0

dec :: IntMap Int -> [Int]
dec = elems

opcode :: IntMap Int -> IntMap Int
opcode zs = opcode' (0, zs)

opcode' :: (Int, IntMap Int) -> IntMap Int
opcode' (i, zs) = 
  case zs ! i of
    99 -> zs
    1  -> opcode' $ opAdd (i, zs)
    2  -> opcode' $ opMult (i, zs)
    _  -> zs

opAdd :: (Int, IntMap Int) -> (Int, IntMap Int)
opAdd = op4 (+)

opMult :: (Int, IntMap Int) -> (Int, IntMap Int)
opMult = op4 (*)

op4 :: (Int -> Int -> Int) -> (Int, IntMap Int) -> (Int, IntMap Int)
op4 f (i, zs) = (i + 4, insert idxOut (f op1 op2) zs)
  where
    idxOp1 = zs ! (i + 1)
    idxOp2 = zs ! (i + 2)
    idxOut = zs ! (i + 3)
    op1    = zs ! idxOp1
    op2    = zs ! idxOp2

op3 :: (Int -> Int) -> (Int, IntMap Int) -> (Int, IntMap Int)
op3 f (i, zs) = (i + 3, insert idxOut (f op1) zs)
  where
    idxOp1 = zs ! (i + 1)
    idxOut = zs ! (i + 2)
    op1    = zs ! idxOp1
