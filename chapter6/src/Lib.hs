{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Lens
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import qualified Data.Map                      as M
import           Data.Maybe                     ( maybeToList )
import Data.Ord (comparing)

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
  [ CrewMember "Grumpy Jappie"     Gunner       ["Juggling", "Arbitrage"]
  , CrewMember "Long-John Newmaid" PowderMonkey ["Origiami"]
  , CrewMember "Salty Chiroptical" PowderMonkey ["Charcuterie"]
  , CrewMember "One-eyed Lumie"    Navigator    []
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
d =
  M.fromList [("Jack", "Captain" :: String), ("Will", "First Mate")]
    ^.. folded
    .   folded

-- 3. Fill in the blanks with the appropriate fold to get the results

e = [1, 2, 3] ^.. folded
-- [1, 2, 3]

f = ("Light" :: String, "Dark") ^.. _1
-- ["Light"]

g = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1
-- ["Light", "Happy"]

h =
  [("Light", "Dark" :: String), ("Happy", "Sad")] ^.. folded . folded . folded

i = ("Bond", "James", "Bond") ^.. each

-- 6.2 Custom Folds

newtype Name =
  Name
    { getName :: String
    } deriving Show

data ShipCrew =
  ShipCrew
    { _shipName :: Name
    , _captain :: Name
    , _firstMate :: Name
    , _conscripts :: [Name]
    } deriving Show

makeLenses ''ShipCrew

crewMembersFold :: Fold ShipCrew Name
crewMembersFold = folding folder
 where
  folder :: ShipCrew -> [Name]
  folder ShipCrew {..} = _captain : _firstMate : _conscripts

myCrew :: ShipCrew
myCrew = ShipCrew
  { _shipName   = Name "Purple Ethanol"
  , _captain    = Name "Grumpy Oren"
  , _firstMate  = Name "Long-John Orc"
  , _conscripts = [Name "One-eyed Whale", Name "Filty Chiroptical"]
  }

j :: [Name]
j = myCrew ^.. crewMembersFold

k :: [String]
k = myCrew ^.. crewMembersFold . to getName

crewNames :: Fold ShipCrew String
crewNames = folding (\s -> s ^.. captain <> s ^.. firstMate <> s ^. conscripts)
  . to getName

l :: [String]
l = myCrew ^.. crewNames

-- Exercises -- Custom Folds

-- 1. Fill in blanks with `folded`, `folding` or `to` (see pg 92)

m = ["Yer" :: String, "a", "wizard", "cojames"] ^.. folded . folded
-- "Yerawizardcojames"

n = [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2)
-- [1, 2, 4, 5]

o = [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2)
-- [[1, 2], [4, 5]]

p = ["lumie", "otto", "hannah"] ^.. folded . to reverse
-- ["lumie", "otto", "hannah"]

q = ("abc", "def") ^.. folding (\(a, b) -> [a, b]) . to reverse . folded
-- "cbafed"

-- 2. Fill in the blank with a path of folds which result in the answer
-- avoid partial functions and fmap

r = [1 .. 5] ^.. folded . to (* 100)
-- [100, 200, 300, 400, 500]

s = (1, 2) ^.. folding (\(a, b) -> [a, b])
-- [1, 2]

t = [(1, "one"), (2, "two")] ^.. folded . _2
-- ["one", "two"]

-- u wasn't in the book, it was an experiment with chat
u = (1, "one" :: String) ^.. folding snd

v = (Just 1, Just 2, Just 3) ^.. folding (\(a, b, c) -> [a, b, c]) . folded
-- [1, 2, 3]

w = [Left 1, Right 2, Left 3] ^.. folded . folded
-- [2]

x =
  [([1, 2], [3, 4]), ([5, 6], [7, 8])]
    ^.. folded
    .   folding (\(a, b) -> [a, b])
    .   folded
-- [1, 2, 3, 4, 5, 6, 7, 8]

y = [1, 2, 3, 4] ^.. folded . to (\x -> if odd x then Left x else Right x)
-- [Left 1, Right 2, Left 3, Right 4]

z = [(1, (2, 3)), (4, (5, 6))] ^.. folded . folding (\(a, (b, c)) -> [a, b, c])
-- [1, 2, 3, 4, 5, 6]

eitherToList :: Either a b -> [b]
eitherToList (Left  _) = []
eitherToList (Right x) = [x]

a' =
  [(Just 1, Left "one"), (Nothing, Right 2)]
    ^.. folded
    .   folding (\(a, b) -> [maybeToList a, eitherToList b])
    .   folded
-- a'' comes from the anwers on pg 363 and is a really cool implementation
a'' = [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding
  (\(a, b) -> a ^.. folded <> b ^.. folded)
-- [1, 2]

b' =
  [(1, "one"), (2, "two")] ^.. folded . folding (\(a, b) -> [Left a, Right b])
-- [Left 1, Right "one", Left 2, Right "two"]

c' = S.fromList ["apricots", "apples"] ^.. folded . folding reverse
-- "selppastocirpa"

d' = [(12, 45, 66), (91, 123, 87)] ^.. folded . _2 . folding (reverse . show)
-- "54321"

e' = [(1, "a" :: String), (2, "b"), (3, "c"), (4, "d")] ^.. folded . folding
  (\(a, b) -> [ b | even a ])
-- ["b", "d"]

-- 6.3 Fold Actions

f' = elemOf folded 3 [1, 2, 3, 4]
-- True

g' = anyOf folded even [1, 2, 3, 4]
-- True

h' = findOf folded even [1, 2, 3, 4]
-- Just 2

i' = findOf folded (> 10) [1, 2, 3, 4]
-- Nothing

j' = has folded []
-- False

k' = has folded [1]
-- True

l' = hasn't folded []
-- True

-- lengthOf
-- sumOf
-- productOf
-- firstOf, preview, (^?)
-- lastOf
-- minimumOf
-- maximumOf

data Actor =
  Actor
    { _nombre :: String
    , _birthYear :: Int
    } deriving (Show, Eq)

makeLenses ''Actor

data TVShow =
  TVShow
    { _title :: String
    , _numEpisodes :: Int
    , _numSeasons :: Int
    , _criticScore :: Double
    , _actors :: [Actor]
    } deriving (Show, Eq)

makeLenses ''TVShow

howIMetYourMother :: TVShow
howIMetYourMother = TVShow
  { _title       = "How I Met Your Mother"
  , _numEpisodes = 208
  , _numSeasons  = 9
  , _criticScore = 8398
  , _actors      = [ Actor "Josh Radnor"         1974
                   , Actor "Cobie Smulders"      1982
                   , Actor "Neil Patrick Harris" 1973
                   , Actor "Alyson Hannigan"     1974
                   , Actor "Jason Segel"         1980
                   ]
  }

buffy :: TVShow
buffy = TVShow
  { _title       = "Buffy the Vampire Slayer"
  , _numEpisodes = 144
  , _numSeasons  = 7
  , _criticScore = 81
  , _actors      = [ Actor "Sarah Michelle Gellar" 1976
                   , Actor "Alyson Hannigan"       1974
                   , Actor "Nicholas Brendon"      1971
                   , Actor "David Boreanaz"        1969
                   , Actor "Anthony Head"          1954
                   ]
  }

tvShows :: [TVShow]
tvShows = [howIMetYourMother, buffy]

m' = sumOf (folded . numEpisodes) tvShows

n' = maximumOf (folded . criticScore) tvShows

o' = _title <$> maximumByOf folded (comparing _criticScore) tvShows

p' = minimumByOf (folded . actors . folded) (comparing _birthYear) tvShows

comparingOf :: Ord a => Lens' s a -> s -> s -> Ordering
comparingOf l = comparing (view l)

p'' = minimumByOf (folded . actors . folded) (comparingOf birthYear) tvShows

-- Folding with effects pg 100
