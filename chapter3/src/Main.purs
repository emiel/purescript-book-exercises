module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)

import Data.AddressBook (Address, Entry, AddressBook, insertEntry, emptyBook, findEntryByAddress, hasName, removeDuplicates)

sampleAddress :: Address
sampleAddress =
  { street: "Fitterstraat"
  , city: "Leiden"
  , state: "Zuid Holland"
  }

sampleAddress' :: Address
sampleAddress' =
  { street: "Rooseveltstraat"
  , city: "Leiden"
  , state: "Zuid Holland"
  }

sampleEntry :: Entry
sampleEntry =
  { firstName: "Emiel"
  , lastName: "van de Laar"
  , address: sampleAddress
  }

sampleEntry' :: Entry
sampleEntry' =
  { firstName: "Emiel"
  , lastName: "van de Laar"
  , address: sampleAddress'
  }

sampleBook :: AddressBook
sampleBook = insertEntry sampleEntry' (insertEntry sampleEntry emptyBook)

main :: Effect Unit
main = do
  logShow $ findEntryByAddress sampleAddress sampleBook
  logShow $ hasName "Emiel" sampleBook
  logShow $ hasName "Foobar" sampleBook
  logShow $ removeDuplicates sampleBook
