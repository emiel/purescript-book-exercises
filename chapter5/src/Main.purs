module Main where

import Prelude hiding (gcd)

import Data.Array (tail)
import Data.Foldable (sum)
import Data.Maybe (fromMaybe, Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Math as Math

{- patterm matching -}
gcd :: Int -> Int -> Int
gcd n 0 = n
gcd 0 m = m
gcd n m =
  if n > m then
    gcd (n - m) m
  else
    gcd n (m - n)

{- guards -}
gcd' :: Int -> Int -> Int
gcd' n 0 = n
gcd' 0 n = n
gcd' n m
  | n > m = gcd' (n - m) m
  | otherwise = gcd' n (m - n)

fromString :: String -> Boolean
fromString "true" = true
fromString _ = false

toString :: Boolean -> String
toString true = "true"
toString false = "false"

{-
  (Easy) Write the factorial function using pattern matching. Hint. Consider the
  two cases zero and non-zero inputs.
-}
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

{-
  (Medium) Look up Pascal's Rule for computing binomial coefficients. Use it to
  write a function which computes binomial coefficients using pattern matching.
-}
binom :: Int -> Int -> Int
binom n 0 = 1
binom n k
  | n < k = 0
binom n k = binom (n - 1) k + binom (n - 1) (k - 1)

binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n k
  | k == 0 = 1
  | n < k = 0
  | otherwise = binomialCoefficient (n - 1) k + binomialCoefficient (n - 1) (k - 1)

isEmpty :: forall a. Array a -> Boolean
isEmpty [] = true
isEmpty _ = false

takeFive :: Array Int -> Int
takeFive [ 0, 1, a, b, _ ] = a * b
takeFive _ = 0

-- showPerson :: { first :: String, last :: String } -> String
showPerson :: forall r. { first :: String, last :: String | r } -> String
showPerson { first: x, last: y } = y <> ", " <> x

type Address
  = { street :: String, city :: String }

type Person
  = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

sortPair :: Array Int -> Array Int
sortPair arr@[ x, y ]
  | x <= y = arr
  | otherwise = [ y, x ]
sortPair arr = arr

{-
  (Easy) Write a function sameCity which uses record patterns to test whether two
  Person records belong to the same city.
-}
sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

{-
  (Medium) What is the most general type of the sameCity function, taking into
  account row polymorphism? What about the livesInLA function defined above?
-}
sameCity' :: forall s t u v. { address :: { city :: String | s } | t } -> { address :: { city :: String | u } | v } -> Boolean
sameCity' { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

livesInLA' :: forall r s. { address :: { city :: String | r } | s } -> Boolean
livesInLA' { address: { city: "Los Angeles" } } = true
livesInLA' _ = false

{-
  (Medium) Write a function fromSingleton which uses an array literal pattern
  to extract the sole member of a singleton array. If the array is not a
  singleton, your function should return a provided default value. Your
  function should have type forall a. a -> Array a -> a
-}
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ x ] = x
fromSingleton default _ = default


lzs :: Array Int -> Array Int
lzs [] = []
lzs xs = case sum xs of
           0 -> xs
           _ -> lzs (fromMaybe [] $ tail xs)

partialFunction :: Boolean -> Boolean
partialFunction = unsafePartial \true -> true

redundantCase :: Boolean -> Boolean
redundantCase true = true
redundantCase false = false
-- redundantCase false = false

data Point = Point
  { x :: Number
  , y :: Number
  }

data Shape = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

exampleLine :: Shape
exampleLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point { x: 0.0, y: 0.0 }

    p2 :: Point
    p2 = Point { x: 100.0, y: 50.0 }

showPoint :: Point -> String
showPoint (Point { x: x, y: y }) =
  "(" <> show x <> ", " <> show y <> ")"

showShape :: Shape -> String
showShape (Circle c r) = showPoint c <> ", " <> show r
showShape (Rectangle c w h) = "TBD"
showShape (Line start end)  = "TBD"
showShape (Text p text) = "TBD"

-- Record pun
origin :: Point
origin = Point {x, y}
  where
    x = 0.0
    y = 0.0

{-
  (Easy) Construct a value of type `Shape` which represents a circle centered
  at the origin with radius `10.0`.
-}
circle :: Shape
circle = Circle origin 10.0

{-
  (Medium) Write a function from `Shape`s to `Shape`s, which scales its
  argument by a factor of `2.0`, center the origin.
-}
scale :: Shape -> Shape
scale (Circle _ r) = Circle origin (r * 2.0)
scale (Rectangle _ w h) = Rectangle origin (w * 2.0) (h * 2.0)
scale (Line _ (Point {x: x, y: y})) = Line origin (Point {x: x * 2.0, y: y * 2.0}) -- XXX
scale (Text _ s) = Text origin s

{-
  (Medium) Write a function which extracts the text from a `Shape`. It should
  return `Maybe String`, and use the `Nothing` constructor if the input is not
  constructed using `Text`.
-}

extractText :: Shape -> Maybe String
extractText (Text _ s) = Just s
extractText _ = Nothing

newtype Pixels = Pixels Number
newtype Inches = Inches Number

type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

{-
  (Medium) Extend the vector graphics library with a new operation `area` which
  computes the area of a `Shape`. For the purposes of this exercise, the area
  of a piece of text is assumed to be zero.
-}

area :: Shape -> Number
area (Circle _ r) = Math.pi * (r * r)
area (Rectangle _ l w) = l * w
area (Line _ _) = 0.0
area (Text _ _) = 0.0

{-
  (Difficult) Extend the `Shape` type with a new data constructor `Clipped`,
  which clips another `Picture` to a rectangle. Extend the `shapeBounds`
  function to compute the bounds of a clipped picture. Note that this makes
  `Shape` into a recursive data type.
-}

main :: Effect Unit
main = do
  logShow $ gcd 5 7
  logShow $ gcd' 5 7
  logShow $ fromString "true"
  logShow $ fromString "foo"
  logShow $ toString true
  logShow $ toString false
  logShow $ binomialCoefficient 4 2
  logShow $ isEmpty []
  logShow $ takeFive [ 0, 1, 2, 3, 4 ]
  logShow $ showPerson { first: "Foo", last: "Bar", extra: "Qux" }
  logShow $ livesInLA { name: "Bob", address: { street: "Rodeo", city: "Los Angeles" } }
  logShow $ sortPair [ 2, 1 ]
  logShow $ sortPair [ 1, 2 ]
  logShow $ sortPair [ 2, 3, 1 ]
  logShow $ sameCity { name: "Bob", address: { street: "Rodeo", city: "Los Angeles" } } { name: "Alice", address: { street: "Sunset", city: "Los Angeles" } }
  logShow $ fromSingleton 99 [ 1 ]
  logShow $ fromSingleton 99 [ 1, 2, 3 ]
  logShow $ lzs [1, 2, 3, 4]
  logShow $ lzs [1, -1, -2, 3]
  logShow $ partialFunction true
  -- logShow $ partialFunction true
  logShow $ showShape $ Circle (Point {x: 2.0, y: 3.0}) 5.5
  logShow $ showPoint origin
  logShow $ showShape circle
  logShow $ showShape <<< scale $ Circle (Point {x: 2.0, y: 3.0}) 5.5
  logShow $ extractText $ Text origin "Hello world!"
  logShow $ extractText $ Circle (Point {x: 2.0, y: 3.0}) 5.5
  logShow $ area $ Circle (Point {x: 2.0, y: 3.0}) 5.5
  logShow $ area $ Rectangle origin 20.0 30.0
