module Stream where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
-- import Data.Monoid (class Monoid, mempty)
import Data.String.CodeUnits as String

{-
class Stream stream element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }
-}

{- with functional dependency -}
class Stream stream element | stream -> element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }

instance streamArray :: Stream (Array a) a where
  uncons = Array.uncons

instance streamString :: Stream String Char where
  uncons = String.uncons

foldStream :: forall l e m. Stream l e => Monoid m => (e -> m) -> l -> m
foldStream f list =
  case uncons list of
    Nothing -> mempty
    Just cons -> f cons.head <> foldStream f cons.tail


-- genericTail xs = map _.tail (uncons xs)
-- map _.tail (uncons "testing")
