module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type AddressBook
  = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

{-- Exercise 1.

Types:
- head :: forall a. List a -> Maybe a
- filter :: forall a. (a -> Boolean) -> List a -> List a
- filterEntry :: Entry -> Boolean
- (<<<) forall b c d a. Semigroupoid a => a c d -> a b c -> a b d
- (>>>) forall a b c d. Semigroupoid a => a b c -> a c d -> a b d

--}
findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

{-- Exercise 2. --}
findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress address = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.address == address

{-- Exercise 3. --}
hasName :: String -> AddressBook -> Boolean
hasName name = not (filter filterEntry >>> null)
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == name || entry.lastName == name

{-- Exercise 4. --}
sameName :: Entry -> Entry -> Boolean
sameName e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy sameName
