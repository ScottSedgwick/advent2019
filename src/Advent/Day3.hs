module Advent.Day3 where

import Data.List (sort, sortOn, intersect)
import Data.List.Split (splitOn)
import qualified Data.IntMap.Strict as M

data Direction
  = Up
  | Dn
  | Lt
  | Rt
  deriving (Show, Eq)

type Posn = Int
type Steps = Int
type Vector = (Direction, Steps)

parse :: String -> [Vector]
parse = map parseVec . splitOn ","

parseVec :: String -> Vector
parseVec ('U':xs) = (Up, read xs :: Int)
parseVec ('D':xs) = (Dn, read xs :: Int)
parseVec ('L':xs) = (Lt, read xs :: Int)
parseVec ('R':xs) = (Rt, read xs :: Int)

toPosn :: (Int, Int) -> Posn
toPosn (x,y) = x * 10000 + y

fromPosn :: Posn -> (Int, Int)
fromPosn p = (p `div` 10000, p `mod` 10000)

type Wireboard = M.IntMap Steps  -- (Map Posn Steps)

wirepath :: Posn -> Steps -> [Vector] -> Wireboard -> Wireboard
wirepath _ _ [] wb = wb
wirepath p s (z:zs) wb = segpath p s z wb `M.union` wirepath (nextpos p z) (nextstep s z) zs wb

segpath :: Posn -> Steps -> Vector -> Wireboard -> Wireboard
segpath p s (d, n) wb = wb `M.union` M.fromList (map (\z -> (nextpos p (d, z), s + z)) [0..n - 1])

nextpos :: Posn -> Vector -> Posn
nextpos p (Up, n) = p + n
nextpos p (Dn, n) = p - n
nextpos p (Lt, n) = p - n * 10000
nextpos p (Rt, n) = p + n * 10000

nextstep :: Steps -> Vector -> Steps
nextstep x (_, n) = x + n

intersection :: Wireboard -> Wireboard -> Wireboard
intersection = M.intersectionWith (+)

manhattan :: Posn -> Int
manhattan p = abs x + abs y
  where 
    (x,y) = fromPosn p

d3p1 :: String -> String -> Int
d3p1 x y = zs
  where
    w1 = wirepath 0 0 (parse x) M.empty
    w2 = wirepath 0 0 (parse y) M.empty
    ps = intersection w1 w2
    xs = sortOn (manhattan . fst) $ M.toList ps
    ys = fst (xs !! 1)
    zs = manhattan ys

d3p2 :: String -> String -> Steps
d3p2 s1 s2 = snd x
  where
    w1 = wirepath 0 0 (parse s1) M.empty
    w2 = wirepath 0 0 (parse s2) M.empty
    ps = intersection w1 w2
    x  = sortOn snd (M.toList ps) !! 1
