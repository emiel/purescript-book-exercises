module Main where

import Prelude
import Control.Apply (lift2)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Data.Array (singleton)

import Data.Either (Either(..))
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, traverse)

{-
  (Easy) Use `lift2` to write lifted versions of the numeric operators `+`,
  `-`, `*` and `/` which work with optional arguments.
-}

addA :: forall a f. Apply f => Semiring a => f a -> f a -> f a
addA x y = lift2 (+) x y

subA :: forall a f. Apply f => Ring a => f a -> f a -> f a
subA x y = lift2 (-) x y

mulA :: forall a f. Apply f => Semiring a => f a -> f a -> f a
mulA x y = lift2 (*) x y

divA :: forall a f. Apply f => EuclideanRing a => f a -> f a -> f a
divA x y = lift2 (/) x y

{-
  (Medium) Convince yourself that the definition of `lift3` given above in
  terms of `<$>` and `<*>` does type check.
-}

lift3' :: forall a b c d f. Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3' f a b c = f <$> a <*> b <*> c

doLift :: Maybe Int
doLift = lift3' (\x y z -> x * y + z) (Just 1) (Just 2) (Just 3)

{-
  (Difficult) Write a function `combineMaybe` which has type `forall a f.
  Applicative f => Maybe (f a) -> f (Maybe a)`. This function takes an
  optional computation with side-effects, and returns a side-effecting
  computation which has an optional result.
-}

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just f) = pure <$> f

{-
  (Medium) Write a `Traversable` instance for the following binary tree data
  structure, which combines side-effects from left-to-right:

  ```haskell
  data Tree a = Leaf | Branch (Tree a) a (Tree a)
  ```

  This corresponds to an in-order traversal of the tree. What about a preorder
  traversal? What about reverse order?
-}

-- See https://en.wikibooks.org/wiki/Haskell/Traversable

data Tree a = Leaf | Branch (Tree a) a (Tree a)

someTree :: Tree Int
someTree = Branch (Branch (Branch Leaf 4 Leaf) 1 (Branch Leaf 5 Leaf)) 2 (Branch Leaf 3 Leaf)

instance showTree :: Show a => Show (Tree a) where
  show Leaf = ""
  show (Branch l x r) = show l <> show x <> show r

instance functorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch l x r) = Branch (map f l) (f x) (map f r)

instance foldableTree :: Foldable Tree where
  foldr f acc Leaf = acc
  foldr f acc (Branch l x r) = foldr f (f x (foldr f acc r)) l

  foldl f acc Leaf = acc
  foldl f acc (Branch l x r) = foldl f (f (foldl f acc l) x) r

  foldMap f (Branch l x r) = foldMap f l <> f x <> foldMap f r
  foldMap f Leaf = mempty

instance traversableTree :: Traversable Tree where
  -- traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
  -- traverse :: forall a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Branch l x r) = go
    where
      -- left branch, node, right branch
      inorder = Branch <$> traverse f l <*> f x <*> traverse f r
      -- node, left branch, right branch
      preorder = (\x' l' r' -> Branch l' x' r') <$> f x <*> traverse f l <*> traverse f r
      -- left branch, right branch, node
      postorder = (\l' r' x' -> Branch l' x' r') <$> traverse f l <*> traverse f r <*> f x

      go = postorder

  sequence = traverse identity

-- Flatten tree (in order)
flattenTree :: forall a. Tree a -> Array a
flattenTree = case _ of
  Leaf -> []
  (Branch l x r) -> flattenTree l <> (singleton x) <> flattenTree r

main :: Effect Unit
main = do
  logShow $ addA (Just 2) (Just 3)
  logShow $ subA (Just 2) (Just 3)
  logShow $ mulA (Just 2) (Just 3)
  logShow $ divA (Just 5.0) (Just 2.0)
  logShow $ show ((Left 3) :: Either Int Int)
  logShow $ map identity someTree
  logShow $ foldr (+) 0 someTree
  logShow $ foldl (+) 0 someTree
  logShow $ flattenTree someTree
  logShow $ traverse (\x -> if x > 0 then Just x else Nothing) someTree
