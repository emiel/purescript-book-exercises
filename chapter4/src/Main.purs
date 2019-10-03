module Main where

import Prelude

import Control.MonadZero (guard)
import Effect (Effect)
import Effect.Console (logShow)

import Data.Array (null, head, tail, (..), filter, length, cons)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl)
import FileOperations (allFiles, allFiles', onlyFiles, onlyFiles', largest, smallest, whereIs)
import Data.Path (root)

{-
  1. (Easy) Write a recursive function which returns true if and only if its input
  is an even integer.
  - 8 (-2) 6 (-2) 4 (-2) 2 (-2) 0 => true
  - 7 (-2) 5 (-2) 3 (-2) 1 => false
-}
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (n - 2)

{-
  2. (Medium) Write a recursive function which counts the number of even integers in
  an array. Hint: the function head (also available in Data.Array) can be used
  to find the first element in a non-empty array.
-}

evenCount :: Array Int -> Int
evenCount arr =
  if null arr
    then 0
    else
      let
          acc = if isEven $ fromMaybe 0 (head arr) then 1 else 0
      in
        acc + evenCount (fromMaybe [] (tail arr))
{-
  (Easy) Use the map or <$> function to write a function which calculates the
  squares of an array of numbers.
-}
squares :: Array Int -> Array Int
squares arr = (\n -> n * n) <$> arr

{-
  (Easy) Use the filter function to write a function which removes the negative
  numbers from an array of numbers.
-}
filterNegative :: Array Int -> Array Int
filterNegative arr = filter (_ > 0) arr

{-
  (Medium) Define an infix synonym <$?> for filter. Rewrite your answer to the
  previous question to use your new operator. Experiment with the precedence
  level and associativity of your operator in PSCi.
-}

infix 200 filter as $?

filterNegative' :: Array Int -> Array Int
filterNegative' arr = (_ > 0) $? arr

{-
  (Easy) Use the factors function to define a function isPrime which tests if its
  integer argument is prime or not.
-}

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1

{-
  (Medium) Write a function which uses do notation to find the cartesian product
  of two arrays, i.e. the set of all pairs of elements a, b, where a is an
  element of the first array, and b is an element of the second.
-}

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  i <- xs
  j <- ys
  pure [i, j]

{-
  (Medium) A _Pythagorean triple_ is an array of numbers `[a, b, c]` such that
  `a² + b² = c²`. Use the `guard` function in an array comprehension to write a
  function `triples` which takes a number `n` and calculates all Pythagorean
  triples whose components are less than `n`. Your function should have type
  `Int -> Array (Array Int)`.
-}

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1..n
  b <- a..n
  c <- b..n
  guard $ (a * a) + (b * b) == (c * c)
  pure [a, b, c]

{-
  (Difficult) Write a function `factorizations` which produces all
  _factorizations_ of an integer `n`, i.e. arrays of integers whose product is
  `n`. _Hint_: for an integer greater than 1, break the problem down into two
  subproblems: finding the first factor, and finding the remaining factors.
-}

factorizations :: Int -> Array (Array Int)
factorizations n = do
  pure [] -- XXX

{-
 (Easy) Use `foldl` to test whether an array of boolean values are all true.
-}
all' :: Array Boolean -> Boolean
all' = foldl (&&) true

{-
  (Medium) Characterize those arrays `xs` for which the function `foldl (==)
  false xs` returns true.
-}
doit :: Array Boolean -> Boolean
doit = foldl (==) false
{-
  doit [false, true, false]
  ((((false == false) == true) == false) -> false
-}

{-
  (Medium) Rewrite the `fib` function in tail recursive form using an accumulator
  parameter:
-}
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: Int -> Int
fib' n = fibAux n 1 0
  where
    fibAux :: Int -> Int -> Int -> Int
    fibAux 0 acc _ = acc
    fibAux 1 acc _ = acc
    fibAux z acc prev = fibAux (z - 1) (acc + prev) acc

{-
  (Medium) Write `reverse` in terms of `foldl`.
-}
reverse' :: forall a. Array a -> Array a
reverse' = foldl (flip $ cons) []


main :: Effect Unit
main = do
  logShow $ isEven 37
  logShow $ isEven 40
  logShow $ isEven 42
  logShow $ isEven 3
  logShow $ evenCount [1,2,3,4,5,6]
  logShow $ evenCount [2,2,2,2,2,2]
  logShow $ evenCount [1,1,1,1,1,1]
  logShow $ evenCount []
  logShow $ squares (1..10)
  logShow $ filterNegative [1, -2, 3, -4, 5]
  logShow $ filterNegative' [1, -2, 3, -4, 5]
  logShow $ isPrime 5
  logShow $ isPrime 100
  logShow $ isPrime 11
  logShow $ isPrime 13
  logShow $ isPrime 14
  logShow $ cartesianProduct [1,2] [3,4]
  logShow $ triples 100
  logShow $ factorizations 12
  logShow $ all' [true, true, true]
  logShow $ all' [false, false]
  logShow $ all' [true, false, true]
  logShow $ doit [true, true, true] -- false
  logShow $ doit [false, false] -- false
  logShow $ doit [true, false, true] -- true
  logShow $ doit [false, true, false] -- false
  logShow $ fib 25
  logShow $ fib' 25
  logShow $ reverse' [1,2,3]
  logShow $ allFiles root
  logShow $ allFiles' root
  logShow $ onlyFiles root
  logShow $ onlyFiles' root
  logShow $ largest root
  logShow $ smallest root
  logShow $ whereIs "/bin/ls"
  logShow $ whereIs "/bin/cat"
