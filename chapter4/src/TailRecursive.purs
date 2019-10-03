module TailRecursive where

import Prelude ((+), (-))
import Data.Array

{-- Not tail recursive --}
z :: Int -> Int
z 0 = 0
z n = 1 + z (n - 1)

{-- Tail recursive! --}
f' :: Int -> Int
f' n = f n 0
  where
    f :: Int -> Int -> Int
    f 0 acc = acc
    f nn acc = f (nn - 1) (acc + 1)


{-- as a fold --}
x :: Int -> Int
x n = foldr (+) 0 (replicate n 1)
