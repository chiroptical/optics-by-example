{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Ord
import qualified Data.Text as T
import GHC.Generics
import Text.RawString.QQ

a = "42" ^? _Double

b = "42" ^? _String

jsonObject =
  [r|
{
  "name": "Jack Sparrow",
  "rank": "Captain"
}
|]

jsonArray =
  [r|
[
  "North",
  "South",
  "East",
  "West"
]
|]

c = jsonObject ^? _Object

d = jsonArray ^? _Array

blackPearl :: String
blackPearl =
  [r|
{
  "name": "Black Pearl",
  "crew": [
    {
      "name": "Jack Sparrow",
      "rank": "Captain"
    },
    {
      "name": "Will Turner",
      "rank": "First Mate"
    }
  ]
}
|]

e = blackPearl ^? _Object . ix (T.pack "crew") . _Array . ix 0 . ix (T.pack "name") . _String

f = blackPearl ^? key (T.pack "crew") . nth 0 . key (T.pack "name") . _String

g = "[\"a\", \"b\", \"c\"]" ^@.. values . _String

fleet :: String
fleet =
  [r|
[
  {
    "name": "Black Pearl",
    "crew": [
      {
        "name": "Jack Sparrow",
        "rank": "Captain"
      },
      {
        "name": "Will Turner",
        "rank": "First Mate"
      }
    ]
  },
  {
    "name": "Flying Dutchman",
    "crew": [
      {
        "name": "Davy Jones",
        "rank": "Captain"
      },
      {
        "name": "Bootstrap Bill",
        "rank": "First Mate"
      }
    ]
  }
]
|]

h = fleet ^.. values . key (T.pack "name") . _String

i =
  fleet
    ^@.. values
      . reindexed (view (key (T.pack "name") . _String)) selfIndex
    <. (key (T.pack "crew") . values . key (T.pack "name") . _String)

cargo :: String
cargo =
  [r|
{
  "emeralds": 327,
  "rubies": 480,
  "sapphires": 621,
  "opals": 92,
  "dubloons": 621
}
|]

j = cargo ^.. members . _Integer

k = sumOf (members . _Integer) cargo

l = cargo ^@.. members . _Integer

m = maximumByOf (members . _Integer . withIndex) (comparing snd) cargo

n =
  fleet
    ^.. values
      . key (T.pack "crew")
      . values
      . filteredBy (key (T.pack "rank") . only (_String # T.pack "Captain"))
      . key (T.pack "name")
      . _String

data Creature = Creature
  { creatureName :: String,
    creatureSize :: Double
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

makeLensesFor
  [ ("creatureName", "creatureNameL"),
    ("creatureSize", "creatureSizeL")
  ]
  ''Creature

creatureSightings :: String
creatureSightings =
  [r|
{
  "Arctic": [
    {
      "creatureName": "Giant Squid",
      "creatureSize": 45.2
    }
  ],
  "Pacific": [
    {
      "creatureName": "Kraken",
      "creatureSize": 124.4
    },
    {
      "creatureName": "Ogopogo",
      "creatureSize": 34.6
    }
  ]
}
|]

o = creatureSightings ^.. members . values . _JSON :: [Creature]

p :: String
p = _JSON # Creature "Kraken" 124.4

q =
  creatureSightings
    & members
      . values
      . _JSON @_ @Creature
      . creatureSizeL
    *~ 1.2
