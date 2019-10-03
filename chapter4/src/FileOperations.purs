module FileOperations where

import Prelude (bind, discard, not, (<), (<<<), (>), ($), (==))

import Control.MonadZero (guard)
import Data.Array (concatMap, (:), filter, head)
import Data.Maybe (Maybe(..))
import Data.Path (Path, ls, isDirectory, size, root, filename)
import Data.Traversable (foldl)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' path = path : do
  child <- ls path
  allFiles' child

{-
  (Easy) Write a function `onlyFiles` which returns all _files_ (not directories)
  in all subdirectories of a directory.
-}
onlyFiles :: Path -> Array Path
onlyFiles path = filter (not <<< isDirectory) (allFiles path)

onlyFiles' :: Path -> Array Path
onlyFiles' path = do
  child <- allFiles' path
  guard (not (isDirectory child))
  [child]

{-
  (Medium) Write a fold to determine the largest and smallest files in the
  filesystem.
-}
largest :: Path -> Maybe Path
largest path = foldl larger Nothing (onlyFiles path)
  where
    larger :: Maybe Path -> Path -> Maybe Path
    larger Nothing p = Just p
    larger (Just acc) p =
      if size acc > size p
        then Just acc
        else Just p

smallest :: Path -> Maybe Path
smallest path = foldl smaller Nothing (onlyFiles path)
  where
    smaller :: Maybe Path -> Path -> Maybe Path
    smaller Nothing p = Just p
    smaller (Just acc) p =
      if size acc < size p
        then Just acc
        else Just p

{-
  (Difficult) Write a function `whereIs` to search for a file by name. The
  function should return a value of type `Maybe Path`, indicating the directory
  containing the file, if it exists. It should behave as follows:

     > whereIs "/bin/ls"
     Just (/bin/)

     > whereIs "/bin/cat"
     Nothing
-}

whereIs :: String -> Maybe Path
whereIs name = head find
  where
    find = do
      path <- allFiles root
      guard $ isDirectory path
      file <- ls path
      guard $ filename file == name
      [path]
