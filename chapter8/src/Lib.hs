{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Tree (Tree (Node))
import qualified Data.Set as S
import Data.Char (toLower)

a = ["Borg", "Cardassian", "Talaxian"]

b = a ^? ix 1

c = a ^? ix 10

c' = a ^. ix 10

d = a & ix 1 .~ "Vulkan"

e = a & ix 10 .~ "Romulan"

f = M.fromList [("Katara", "Water"), ("Toph", "Earth"), ("Zuko", "Fire")]

g = f ^? ix "Zuko"

h = f ^? ix "Sokka"

i = ("hello" :: T.Text) ^? ix 0

j = ("hello" :: BS.ByteString) ^? ix 0

k = ("hello" :: BS.ByteString) & ix 0 +~ 2

l = ("hello" :: T.Text) & ix 1 %%~ \_ -> ("aeiou" :: [Char])

m :: Tree Integer
m = Node 1 [Node 2 [Node 4 []], Node 3 [Node 5 [], Node 6 [], Node 7 []]]

-- Index of first subtree, index of second subtree
n = m ^? ix [1, 2]

o = reverse ^? ix "Stella!"

p = reverse & ix ("password" :: String) .~ "You guessed the magic word!"

q = f ^. at "Zuko"

r = f & at "Zuko" .~ Nothing

s = f & at "Hello" .~ Just "World"

s' = f & at "Hello" ?~ "World"

primes :: S.Set Integer
primes = S.fromList [2, 3, 5, 7, 11, 13]

t = primes ^? ix 5

u = primes ^? ix 4

v = primes & at 14 ?~ ()

-- Exercises - Indexable Structures

-- 1. Fill in the blanks

w = ["Larry", "Curly", "Moe"] & ix 1 .~ "Wiggly"
-- ["Larry", "Wiggly", "Moe"]

hv = M.fromList [("Superman", "Lex Luther"), ("Batman", "Joker")]

x = hv & at "Spiderman" .~ Just "Goblin"
-- M.fromList [("Superman", "Lex Luther"), ("Spiderman", "Goblin"), ("Batman", "Joker")]

y = sans "Superman" hv 
-- M.fromList [("Batman", "Joker")]

z = S.fromList ['a', 'e', 'i', 'o', 'u'] & at 'y' ?~ () & at 'i' .~ Nothing
-- S.fromList "aeouy"

-- 2. Use `ix` and `at` to go from input to output

input = M.fromList [("candy bars", 13), ("soda", 34), ("gum", 7)]
output = M.fromList [("candy bars", 13), ("ice cream", 5), ("soda", 37)]

a' = input
  & at "gum" .~ Nothing
  & at "ice cream" ?~ 5
  & at "soda" %~ fmap (+3)

newtype Cycled a = Cycled [a] deriving Show

type instance Index (Cycled a) = Int
type instance IxValue (Cycled a) = a

instance Ixed (Cycled a) where
  -- ix :: Int -> Traversal' (Cycled a) a
  ix :: Applicative f => Int -> (a -> f a) -> Cycled a -> f (Cycled a)
  ix i handler (Cycled xs) =
    Cycled <$> (traverseOf (ix (i `mod` length xs)) handler xs)

b' = Cycled ['a', 'b', 'c'] & ix (-1) .~ '!'

data Address =
  Address
    { _buildingNumber :: Maybe String
    , _streetName :: Maybe String
    , _apartmentNumber :: Maybe String
    , _postalCode :: Maybe String
    } deriving Show

makeLenses ''Address

data AddressComponent
  = BuildingNumber
  | StreetName
  | ApartmentNumber
  | PostalCode
  deriving Show

type instance Index Address = AddressComponent
type instance IxValue Address = String

instance Ixed Address

instance At Address where
  at :: AddressComponent -> Lens' Address (Maybe String)
  at BuildingNumber = buildingNumber
  at StreetName = streetName
  at ApartmentNumber = apartmentNumber
  at PostalCode = postalCode

addr = Address Nothing Nothing Nothing Nothing

d' = addr
  & at StreetName ?~ "Baker St."
  & at ApartmentNumber ?~ "221B"

-- Exercises - Custom Indexed Structures

-- 1. Implement both `Ixed` and `At` for a newtype wrapper around `Map`
--    which makes indexing case insensitive

newtype CIMap a = CIMap (M.Map String a) deriving Show

type instance Index (CIMap a) = String
type instance IxValue (CIMap a) = a

instance Ixed (CIMap a) where
  ix :: Applicative f => String -> (a -> f a) -> CIMap a -> f (CIMap a)
  ix i handler (CIMap map) =
    CIMap <$> (traverseOf (ix (toLower <$> i)) handler map)

f' = CIMap (M.fromList [("hello", 0)]) ^? ix "hello"

f_ = CIMap (M.fromList [("hello", 0)]) ^? ix "world"

instance At (CIMap a) where
  at :: Functor f => String -> (Maybe a -> f (Maybe a)) -> CIMap a -> f (CIMap a)
  at key handler (CIMap map) = CIMap <$> (map & at (toLower <$> key) handler)

g' = CIMap (M.empty) & at "Hello" ?~ "world"
g_ = g' ^? ix "HELLO"
