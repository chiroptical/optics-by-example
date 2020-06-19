{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           Control.Lens
import qualified Data.Map                      as M
import           Data.Tree
import           Data.Char

-- 7.1 Introduction to Traverals

-- > All lenses are valid traversals but the reverse isn't true

a = ("Bubbles", "Buttercup") & both %~ (++ "!")

b = ("Bubbles", "Buttercup") & both .~ "Blossom"

c = ("Bubbles", "Buttercup") & both %~ length

d = ("Bubbles", "Buttercup", "Blossom") & each %~ length

e = [1 .. 5] & taking 3 traversed *~ 10

f = [1 .. 5] & dropping 3 traversed *~ 10

g =
  "once up a time - optics became mainstream"
    &  takingWhile (/= '-') traversed
    %~ toUpper

h = [1 .. 5] & traversed . filtered even *~ 10

i = ("short", "much longer") & both . filtered ((> 5) . length) %~ reverse

-- 7.2 Traversal Combinators

j = [1 .. 3] & traversed *~ 10

k = ("Batman", "Superman") & traversed %~ take 3

powerLevels = M.fromList
  [ ("Gohan"  , 710)
  , ("Goku"   , 9001)
  , ("Vegeta" , 8000)
  , ("Krillin", 5000)
  , ("Piccolo", 408)
  ]

l = powerLevels & traversed %~ \n -> if n > 9000 then "Over 9000" else show n

opticsTree = Node "Lens" [Node "Fold" [], Node "Traversal" []]

m = opticsTree & traversed %~ reverse

n = "I'll be back!" ^.. worded & traversed .~ "hello"
-- ["hello","hello","hello"]
o = "I'll be back!" & worded .~ "hello"
-- "hello hello hello"

p = "blue suede shoes" & worded . _head %~ toUpper
q = "blue suede shoes" & worded %~ \(x : xs) -> toUpper x : xs

r = ("T-Rex", (42, "Stegosaurus")) & beside id _2 <>~ "!"

s = ([(1, 2), (3, 4)], [5, 6, 7]) & beside (traversed . both) traversed +~ 1

t = ([(1, 2), (3, 4)], [5, 6, 7]) ^.. beside (traversed . both) traversed

u = Left (1, 2) & beside both id %~ negate

v = Right [1, 2] & beside id traversed %~ negate

w = "blue suede shoes" & elementOf worded 1 .~ "leather"

-- 7.3 Traversal Composition

-- Exercises

-- 1. Short answer
-- - What type of optic do you get when compose a traversal with a fold?
-- A fold
--
-- - Which of the optics we've learned can act as a traversal?
-- Lens or a Traversal
--
-- - Which of the optics we've learned can act as a fold?
-- A Lens, Fold, or Traversal

-- 2. Fill in the blank to complete each expression:

x = ("Jurassic", "Park") & both .~ "N/A"
-- ("N/A", "N/A")

y = ("Jurassic", "Park") & both . traversed .~ 'x'
-- ("xxxxxxxx", "xxxx")

z = ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & beside id traversed %~ take 3
-- ("Mal", ["Kay", "Ina", "Jay"])

a' = ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & _2 . element 1 .~ "River"
-- ("Malcolm", ["Kaylee", "River", "Jayne"])

b' =
  ["Die Another Day", "Live and Let Die", "You Only Live Twice"]
    &  traversed
    .  elementOf worded 1
    .  traversed
    .~ 'x'
-- [ "Die xxxxxxx Day"
-- , "Live xxx Let Die"
-- , "You xxxx Live Twice"
-- ]

c' = ((1, 2), (3, 4)) & each . each +~ 1
-- ((2, 3), (4, 5))

d' = (1, (2, [3, 4])) & beside id (beside id traversed) +~ 1
-- (2, (3, [4, 5]))

e' =
  ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
    &  each
    .  filteredBy (_1 . only True)
    .  _2
    .  taking 5 traversed
    %~ toUpper
-- ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))

f' =
  ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
    &  each
    %~ snd
-- ("Strawberries", "Blueberries", "Blackberries")

-- 19.16 Traversal Actions
