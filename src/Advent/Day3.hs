module Advent.Day3 where

import Data.List (sortOn, intersect)
import Data.List.Split (splitOn)
import qualified Data.IntMap.Strict as M

data Vector
  = Up Int
  | Dn Int
  | Lt Int
  | Rt Int
  deriving (Show, Eq)

parse :: String -> [Vector]
parse = map parseVec . splitOn ","

parseVec :: String -> Vector
parseVec ('U':xs) = Up (read xs :: Int)
parseVec ('D':xs) = Dn (read xs :: Int)
parseVec ('L':xs) = Lt (read xs :: Int)
parseVec ('R':xs) = Rt (read xs :: Int)

type Posn = Int
type Steps = Int

toPosn :: (Int, Int) -> Posn
toPosn (x,y) = x * 10000 + y

fromPosn :: Posn -> (Int, Int)
fromPosn p = (p `div` 10000, p `mod` 10000)

type Wireboard = M.IntMap Steps

wirepath :: Posn -> Steps -> [Vector] -> Wireboard -> Wireboard
wirepath _ _ [] wb = wb
wirepath p s (z:zs) wb = segpath p s z wb `M.union` wirepath (nextpos p z) (nextstep s z) zs wb

segpath :: Posn -> Steps -> Vector -> Wireboard -> Wireboard
segpath p s (Up n) wb = wb `M.union` M.fromList (map (\z -> (p + z, s + z)) [0..n - 1])
segpath p s (Dn n) wb = wb `M.union` M.fromList (map (\z -> (p - z, s + z)) [0..n - 1])
segpath p s (Lt n) wb = wb `M.union` M.fromList (map (\z -> (p - z * 10000, s + z)) [0..n - 1])
segpath p s (Rt n) wb = wb `M.union` M.fromList (map (\z -> (p + z * 10000, s + z)) [0..n - 1])

nextpos :: Posn -> Vector -> Posn
nextpos p (Up n) = p + n
nextpos p (Dn n) = p - n
nextpos p (Lt n) = p - n * 10000
nextpos p (Rt n) = p + n * 10000

nextstep :: Steps -> Vector -> Steps
nextstep x (Up n) = x + n
nextstep x (Dn n) = x + n
nextstep x (Lt n) = x + n
nextstep x (Rt n) = x + n

intersection :: Wireboard -> Wireboard -> Wireboard
intersection = M.intersectionWith (+)

distance :: [(Int, Int)] -> Int
distance = dist . fst . nearest order1

order1 :: [(Posn, Steps)] -> [(Posn, Steps)]
order1 = sortOn (dist . fst)

dist :: Posn -> Int
dist p = abs x + abs y
  where 
    (x,y) = fromPosn p

d3p1 :: String -> String -> Int
d3p1 x y = distance (M.toList ps)
  where
    w1 = wirepath 0 0 (parse x) M.empty
    w2 = wirepath 0 0 (parse y) M.empty
    ps = intersection w1 w2

d3p2 :: String -> String -> Steps
d3p2 s1 s2 = snd x
  where
    w1 = wirepath 0 0 (parse s1) M.empty
    w2 = wirepath 0 0 (parse s2) M.empty
    ps = intersection w1 w2
    x  = nearest order2 (M.toList ps)

order2 :: [(Int, Int)] -> [(Int, Int)]
order2 = sortOn snd

nearest ::([(Int, Int)] -> [(Int, Int)]) -> [(Int, Int)] -> (Int, Int)
nearest f xs = f xs !! 1