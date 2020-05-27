{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Lens
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Map as M

data Role
  = Gunner
  | PowderMonkey
  | Navigator
  | Captain
  | FirstMate
  deriving (Show, Eq, Ord)

data CrewMember =
  CrewMember
    { _name :: String
    , _role :: Role
    , _talents :: [String]
    } deriving (Show, Eq, Ord)

makeLenses ''CrewMember

roster :: S.Set CrewMember
roster = S.fromList
  [ CrewMember "Grumpy Jappie" Gunner ["Juggling", "Arbitrage"]
  , CrewMember "Long-John Newmaid" PowderMonkey ["Origiami"]
  , CrewMember "Salty Chiroptical" PowderMonkey ["Charcuterie"]
  , CrewMember "One-eyed Lumie" Navigator []
  ]

crewMembers :: Fold (S.Set CrewMember) CrewMember
crewMembers = folded

a :: [CrewMember]
a = roster ^.. folded

rosterRoles :: Fold (S.Set CrewMember) Role
rosterRoles = folded . role

-- Exercises -- Simple Folds

-- 1. What are results of each expression

beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

-- >>> beastSizes ^.. folded
-- [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

-- >>> beastSizes ^.. folded . folded
-- ["Sirens", "Kraken", "Ogopogo"]

-- >>> beastSizes ^.. folded . folded . folded
-- "SirenKrakenOgopogo"

-- >>> beastSizes ^.. folded . _2
-- ["Sirens", "Kraken", "Ogopogo"]

-- >>> toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]
-- >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . folded
-- [1, 2, 3, 4, 5, 6]

-- >>> M.fromList [("Jack", "Captain"), ("Will", "First Mate")] ^.. folded . folded
-- "CaptainFirst Mate"

-- >>> ("Hello", "It's me") ^.. both . folded
-- "HelloIt's me"

-- >>> ("Why", "So", "Serious?") ^.. each
-- ["Why", "So", "Serious?"]

quotes :: [(T.Text, T.Text, T.Text)]
quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]

-- >>> quotes ^.. each . each . each
-- ['W', 'h', 'y', ...] aka "WhySo..."

-- 2. Write out ``specialized'' type for each of the requested combinators

-- folded :: Fold [(Int, Char)] (Int, Char)
-- _1 :: Fold (Int, Char) Int
b = [(1, 'a'), (2, 'b'), (3, 'c')] ^.. folded . _1

-- folded :: Fold (S.Set String) String
-- _2 :: Fold (Bool, S.Set String) (S.Set String)
-- (^..) :: Fold (Bool, S.Set String) String -> (Bool, S.Set String) -> [String]
c = (False, S.fromList ["one", "two", "three"]) ^.. _2 . folded

-- folded :: Fold (M.Map String String) String
-- folded :: Fold String Char
-- (^..) :: Fold (M.Map String String) Char -> M.Map String String -> String
d = M.fromList [("Jack", "Captain" :: String), ("Will", "First Mate")]
      ^.. folded . folded

-- 3. Fill in the blanks with the appropriate fold to get the results

e = [1, 2, 3] ^.. folded
-- [1, 2, 3]

f = ("Light" :: String, "Dark") ^.. _1
-- ["Light"]

g = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1
-- ["Light", "Happy"]

h = [("Light", "Dark" :: String), ("Happy", "Sad")] ^.. folded . folded . folded

i = ("Bond", "James", "Bond") ^.. each
