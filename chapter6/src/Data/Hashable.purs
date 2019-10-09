module Data.Hashable
  ( HashCode
  , hashCode
  , class Hashable
  , hash
  , hashEqual
  , combineHashes
  , arrayDups
  ) where

import Prelude

import Data.Array (nubByEq, length)
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
  hash :: a -> HashCode

instance showHashCode :: Show HashCode where
  show (HashCode h) = "(HashCode " <> show h <> ")"

derive instance eqHashCode :: Eq HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashMaybe :: Hashable a => Hashable (Maybe a) where
  hash Nothing = hashCode 0
  hash (Just a) = hashCode 1 `combineHashes` hash a

instance hashTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
  hash (Tuple a b) = hash a `combineHashes` hash b

instance hashEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left a) = hashCode 0 `combineHashes` hash a
  hash (Right b) = hashCode 1 `combineHashes` hash b

{-
  (Easy) Use PSCi to test the hash functions for each of the defined instances.
-}

{-
  (Medium) Use the `hashEqual` function to write a function which tests if an
  array has any duplicate elements, using hash-equality as an approximation to
  value equality. Remember to check for value equality using `==` if a duplicate
  pair is found. _Hint_: the `nubBy` function in `Data.Array` should make this
  task much simpler.
-}

arrayDups :: forall a. Hashable a => Array a -> Boolean
arrayDups [] = false
arrayDups xs = length xs /= length (nubByEq myEq xs)
  where
    myEq x y = hashEqual x y && x == y

{-
  (Medium) Write a `Hashable` instance for the following newtype which
  satisfies the type class law:

  ```haskell
  newtype Hour = Hour Int

  instance eqHour :: Eq Hour where
    eq (Hour n) (Hour m) = mod n 12 == mod m 12
  ```

  The newtype `Hour` and its `Eq` instance represent the type of integers
  modulo 12, so that 1 and 13 are identified as equal, for example. Prove that
  the type class law holds for your instance.
-}

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

{-
  XXX

instance hashableHour :: Hashable Hour where
  hash (Hour n) = hashCode n
-}

{-
  (Difficult) Prove the type class laws for the `Hashable` instances for
  `Maybe`, `Either` and `Tuple`.
-}

-- XXX
