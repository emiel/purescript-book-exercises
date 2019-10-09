module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Data.Array ((:))
import Data.Foldable (class Foldable, foldr, foldl, foldMap, maximum)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Data.Hashable (arrayDups)

data Point = Point
  { x :: Number
  , y :: Number
  }

data Shape = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

{-
  (Easy) Use the showShape function from the previous chapter to define a Show
  instance for the Shape type.
-}

instance showPoint :: Show Point where
  show (Point {x, y}) =
    "(" <> show x <> ", " <> show y <> ")"

instance showShape :: Show Shape where
  show (Circle c r) =
    "Circle [center: " <> show c <> ", radius: " <> show r <> "]"
  show (Rectangle c w h) =
    "Rectangle [center: " <> show c <> ", width: " <> show w <> ", height: " <> show h <> "]"
  show (Line start end) =
    "Line [start: " <> show start <> ", end: " <> show end <> "]"
  show (Text loc text) =
    "Text [location: " <> show loc <> ", text: " <> show text <> "]"

{-
  (Easy) The following newtype represents a complex number:

     ```haskell
     newtype Complex = Complex
       { real :: Number
       , imaginary :: Number
       }
     ```

  Define `Show` and `Eq` instances for `Complex`.
-}

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) =
    "(" <> show real <> ", " <> show imaginary <> ")"

instance eqComplex :: Eq Complex where
  eq (Complex lr) (Complex rr) = lr.real == rr.real && lr.imaginary == rr.imaginary

threeAreEqual :: forall a. Eq a => a -> a -> a -> Boolean
threeAreEqual a1 a2 a3 = a1 == a2 && a2 == a3

showCompare :: forall a. Ord a => Show a => a -> a -> String
showCompare a1 a2 | a1 < a2 =
  show a1 <> " is less than " <> show a2
showCompare a1 a2 | a1 > a2 =
  show a1 <> " is greater than " <> show a2
showCompare a1 a2 =
  show a1 <> " is equal to " <> show a2

{-
  (Easy) The following declaration defines a type of non-empty arrays of
  elements of type `a`:

    ```haskell
    data NonEmpty a = NonEmpty a (Array a)
    ```

  Write an `Eq` instance for the type `NonEmpty a` which reuses the instances
  for `Eq a` and `Eq (Array a)`.
-}

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty x xs) = show x <> ", " <> show xs

{-
  (Medium) Write a `Semigroup` instance for `NonEmpty a` by reusing the
  `Semigroup` instance for `Array`.
-}

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)

{-
  (Medium) Write a `Functor` instance for `NonEmpty`.
-}

instance functorNonEmpty :: Functor NonEmpty where
  map fn (NonEmpty x xs) = NonEmpty (fn x) (map fn xs)

{-
  (Medium) Given any type `a` with an instance of `Ord`, we can add a new
  "infinite" value which is greater than any other value:

     ```haskell
     data Extended a = Finite a | Infinite
     ```

  Write an `Ord` instance for `Extended a` which reuses the `Ord` instance
  for `a`.
-}

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite x) (Finite y) = x == y
  eq Infinite Infinite = true
  eq _ _ = false

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite x) (Finite y) = compare x y

{-
  (Difficult) Write a `Foldable` instance for `NonEmpty`. _Hint_: reuse the
  `Foldable` instance for arrays.
-}

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f acc (NonEmpty x xs) = foldr f acc (x : xs)
  foldl f acc (NonEmpty x xs) = foldl f acc (x : xs)
  foldMap f (NonEmpty x xs) = append (f x) (foldMap f xs)

{-
  (Difficult) Given an type constructor `f` which defines an ordered container
  (and so has a `Foldable` instance), we can create a new container type which
  includes an extra element at the front:

     ```haskell
     data OneMore f a = OneMore a (f a)
     ```

  The container `OneMore f` also has an ordering, where the new element comes
  before any element of `f`. Write a `Foldable` instance for `OneMore f`:

     ```haskell
     instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
     ...
     ```
-}

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f acc (OneMore x xs) = f x  (foldr f acc xs)
  foldl f acc (OneMore x xs) = foldl f (f acc x) xs
  foldMap f (OneMore x xs) = append (f x) (foldMap f xs)

{-
  (Medium) Define a partial function which finds the maximum of a non-empty array
  of integers. Your function should have type Partial => Array Int -> Int. Test
  out your function in PSCi using unsafePartial. Hint: Use the maximum function
  from Data.Foldable.
-}

maxP :: Partial => Array Int -> Int
maxP xs = fromJust (maximum xs)

{-
  (Medium) The `Action` class is a multi-parameter type class which defines an
  action of one type on another:

  ```haskell
  class Monoid m <= Action m a where
    act :: m -> a -> a
  ```

  An _action_ is a function which describes how monoidal values can be used to
  modify a value of another type. There are two laws for the `Action` type
  class:

  - `act mempty a = a`
  - `act (m1 <> m2) a = act m1 (act m2 a)`

  That is, the action respects the operations defined by the `Monoid` class.

  For example, the natural numbers form a monoid under multiplication:

  ```haskell
  newtype Multiply = Multiply Int

  instance semigroupMultiply :: Semigroup Multiply where
    append (Multiply n) (Multiply m) = Multiply (n * m)

  instance monoidMultiply :: Monoid Multiply where
    mempty = Multiply 1
  ```

  This monoid acts on strings by repeating an input string some number of
  times. Write an instance which implements this action:

  ```haskell
  instance repeatAction :: Action Multiply String
  ```

  Does this instance satisfy the laws listed above?
-}

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

-- XXX instance repeatAction :: Action Multiply String

{-
  (Medium) Write an instance `Action m a => Action m (Array a)`, where the
  action on arrays is defined by acting on each array element independently. 
-}

-- XXX

{-
  (Difficult) Given the following newtype, write an instance for `Action m
  (Self m)`, where the monoid `m` acts on itself using `append`:

  ```haskell
  newtype Self m = Self m
  ```
-}

newtype Self m = Self m

-- XXX

{-
  (Difficult) Should the arguments of the multi-parameter type class `Action` be
  related by some functional dependency? Why or why not?
-}

-- XXX

main :: Effect Unit
main = do
  log <<< show $ Circle (Point {x: 2.0, y: 3.0}) 5.5
  log <<< show $ Text (Point {x: 2.0, y: 3.0}) "Hello world!"
  log <<< show $ (Complex {real: 2.0, imaginary : 3.0})
  log <<< show $ Complex {real: 2.0, imaginary: 3.0} == Complex {real: 2.0, imaginary: 3.0}
  log <<< show $ Complex {real: 2.0, imaginary: 3.0} == Complex {real: 0.0, imaginary: 200.0}
  log <<< show $ threeAreEqual 1 1 1
  log <<< show $ showCompare 'A' 'Z'
  log <<< show $ (NonEmpty 1 []) == (NonEmpty 1 [])
  log <<< show $ (NonEmpty 1 []) == (NonEmpty 2 [])
  -- log <<< show $ (NonEmpty 1 [2, 3]) <> (NonEmpty 4 [5, 6])
  log <<< show $ map show (NonEmpty 1 [2, 3])
  log <<< show $ Finite 3 < Infinite -- true
  log <<< show $ Infinite < Finite 3 -- false
  log <<< show $ Finite 3 == Finite 3 -- true
  log <<< show $ foldr (+) 0 (NonEmpty 1 [2, 3])
  log <<< show $ foldr (+) 0 (OneMore 1 (Just 2))
  log <<< show $ foldr (*) 1 (OneMore 1 [2, 3, 4])
  log <<< show $ unsafePartial $ maxP [1,2,3] -- 3
  log <<< show $ arrayDups [1, 2, 3, 2] -- true
  log <<< show $ arrayDups [1, 1, 1, 1] -- true
  log <<< show $ arrayDups [1, 2, 3, 4] -- false
