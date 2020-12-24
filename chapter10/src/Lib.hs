{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import Control.Lens
import Data.Char (isUpper, toLower, toUpper)
import Data.List (intercalate, transpose)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Lens

packed :: Iso' String Text
packed = iso to' from'
  where
    to' :: String -> Text
    to' = T.pack
    from' :: Text -> String
    from' = T.unpack

a :: Text
a = "Caramba!" ^. packed

b :: String
b = packed # "Caramba!"

c :: String
c = "Caramba" ^. from packed

d :: String
d = "hello" & packed %~ T.toUpper

e :: Text
e = "HELLO" & from packed . traversed %~ toLower

f = ("Fall", "Trust") ^. swapped

-- ("Trust", "Fall")

g = Right "Field" ^. swapped

-- Left "Field"

h =
  let addTuple = (+) ^. uncurried
   in addTuple (1, 2)

i = 100 ^. adding 50

j = adding 50 # 150

k = "Winter is coming" ^. from packed . reversed

-- Exercises - Intro to Isos

-- For each task, choose whether it's best suited to a Lens, Traversal, Prism, or Iso

-- a. Focus a Celsius temperature in Fahrenheit
-- Iso

-- b. Focus the last element of a list
-- Traversal

-- c. View a JSON object as its corresponding Haskell Record
-- Prism

-- d. Rotate the elements of a three-tuple one to the right
-- Iso

-- e. Focus on the ‘bits’ of an Int as Bools
-- Traversal

-- f. Focusing an IntSet from a Set Int
-- Iso

-- 2. Fill in the blanks

l = ("Beauty", "Age") ^. swapped

-- ("Age", "Beauty")

m = 50 ^. from (adding 10)

-- 40

n = 0 & multiplying 4 %~ (+ 12)

-- 3.0

-- n / 2 - 10 = 2
o = 0 & adding 10 . multiplying 2 .~ 24

-- 2

p = [1, 2, 3] & reversed %~ tail

-- [1, 2]

q = ((++) ^. flipped) [1, 2] [3, 4]

-- [3, 4, 1, 2]

r = [1, 2, 3] ^. reversed

-- [3, 2, 1]

s = [[1, 2, 3], [10, 20, 30]] & involuted transpose %~ tail

-- [[2, 3], [20, 30]]

switchCase' c = if isUpper c then toLower c else toUpper c

switchCase :: Iso' String String
switchCase = involuted (fmap switchCase')

t = (32, "Hi") & _2 . switchCase .~ ("hELLO" :: String)

-- (32, "Hello")

u = switchCase # ("hELLO" :: String)

celsiusToF :: Double -> Double
celsiusToF c = (c * 9 / 5) + 32

fahrenheit :: Iso' Double Double
fahrenheit = iso celsiusToF from'
  where
    from' :: Double -> Double
    from' f = f ^. subtracting 32 . dividing (9 / 5)

-- from' f = (f - 32) * 5 / 9

toYamlList :: [String] -> String
toYamlList xs = "- " <> intercalate "\n- " xs

shoppingList :: [Text]
shoppingList = ["Milk", "Eggs", "Flour"]

v :: String
v = shoppingList ^. mapping (from packed) . to toYamlList

-- dimapping contramaps over its input and maps over its output
-- at the same time, i.e.
-- dimapping {First} {Second}
-- Input: [String]
-- Contramap: Text -> String i.e. from packed
-- First: mapping (from packed)
-- Output: String
-- Map: String -> Text
-- Second: packed
textToYamlList :: [Text] -> Text
textToYamlList = toYamlList ^. dimapping (mapping (from packed)) packed

-- Exercises - Projected Isos

-- 1. Fill in the blanks

w = ("Beauty", "Age" :: String) ^. mapping reversed . swapped

-- ("egA","Beauty")

x = [True, False, True] ^. mapping (involuted not)

-- [False, True, False]

y = [True, False, True] & mapping (involuted not) %~ filter id

-- [False]

z = (show ^. mapping reversed) 1234

-- "4321"

-- 2. Using the enum iso, implement intNot using dimapping
-- enum :: Enum a => Iso' Int a

-- dimapping {First} {Second}
-- Input: Bool
-- Contramap: Int -> Bool
-- First: toEnum i.e. enum
--
-- Output: Bool
-- Map: Bool -> Int
-- Second: fromEnum i.e. from enum

intNot :: Int -> Int
intNot = not ^. dimapping enum (from enum)

intNot' :: Int -> Int
intNot' = enum %~ not

newtype Email = Email String
  deriving (Show)

makeWrapped ''Email

newtype UserID = UserID String
  deriving (Show)

a' :: Email
a' = Email "joe@example.com" & coerced %~ reverse @Char

-- over coerced (reverse :: String -> String) $ Email "joe@example.com"

b' = Email "joe@example.com" & _Wrapping' Email %~ reverse

-- Exercises - Iso Laws

-- 1. Iso is unlawful, provide a counter example which shows that it
-- breaks the law

mapList :: Ord k => Iso' (Map k v) [(k, v)]
mapList = iso M.toList M.fromList

c' = [(1, 2), (1, 3)] ^. from mapList . mapList

-- [(1, 3)]

-- 2. Is there a lawful implementation of the following iso

nonEmptyList :: Iso [a] [b] (Maybe (NonEmpty a)) (Maybe (NonEmpty b))
nonEmptyList = iso from' to'
  where
    from' :: [a] -> Maybe (NonEmpty a)
    from' = N.nonEmpty

    to' :: Maybe (NonEmpty b) -> [b]
    to' Nothing = []
    to' (Just nel) = N.toList nel

d' =
  let nel :: Maybe (NonEmpty Integer)
      nel = pure . pure $ 1
   in nel ^. (from nonEmptyList . nonEmptyList)

e' =
  let nel :: Maybe (NonEmpty Integer)
      nel = Nothing
   in nel ^. (from nonEmptyList . nonEmptyList)

f' = [] ^. nonEmptyList . from nonEmptyList

g' = [1] ^. nonEmptyList . from nonEmptyList

sorted :: (Ord a) => Iso' [a] [(Int, a)]
sorted = iso to' from'
  where
    to' xs = L.sortOn snd $ zip [0 ..] xs
    from' xs = snd <$> L.sortOn fst xs

h' = [(10, 'a'), (0, 'b')] ^. from sorted . sorted

-- [(1,'a'),(0,'b')], which is unlawful :(

i' = [1, 2, 3] ^. sorted . from sorted

-- [1, 2, 3] :thumbsup:
