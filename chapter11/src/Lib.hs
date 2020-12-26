{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Control.Lens
import Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Tree as T

a = ["Summer", "Fall", "Winter", "Spring"] ^.. itraversed

-- toListOf itraversed ["Summer", "Fall", "Winter", "Spring"]

b = ["Summer", "Fall", "Winter", "Spring"] ^@.. itraversed

agenda = M.fromList [("Monday", "Shopping"), ("Tuesday", "Swimming")]

c = agenda ^@.. itraversed

-- itraversed will add keys as indexes, whereas traversed with add Int indices

d = (True, "value") ^@.. itraversed

e =
  let t = T.Node "top" [T.Node "left" [T.Node "left'" []], T.Node "right" []]
   in t ^@.. itraversed

-- itraversed with yield [{0,1}] indicating the path, traversed yields number

f =
  let derp =
        M.fromList
          [ ("Monday", ["Shopping", "Yoga"]),
            ("Saturday", ["Brunch", "Food coma"])
          ]
   in derp ^@.. itraversed <. itraversed

-- Using `.` will yield 0,1,0,1 because focus is a list over two keys
-- Using `<.` will use the index on the left i.e. the keys
-- Using `<.>` will combine the keys and numeric indices from `.`

populationMap :: M.Map String (M.Map String Int)
populationMap =
  M.fromList
    [ ("Canada", M.fromList [("Ottawa", 994837), ("Toronto", 2930000)]),
      ("Germany", M.fromList [("Berlin", 3748000), ("Munich", 1456000)])
    ]

-- Combine string indexes together, i.e. outer with inner key for populationMap
(.++) ::
  (Indexed String s t -> r) ->
  (Indexed String a b -> s -> t) ->
  Indexed String a b ->
  r
(.++) = icompose (\a b -> a ++ ", " ++ b)

g = populationMap ^@.. itraversed .++ itraversed

-- Exercises - Indexed Optics

h = M.fromList [("streamResponse", False), ("useSSL", True)] ^@.. itraversed

-- [("streamResponse",False),("useSSL",True)]

i = (M.fromList [('a', 1), ('b', 2)], M.fromList [('c', 3), ('d', 4)]) ^@.. each . itraversed

-- [('a',1),('b',2),('c',3),('d',4)]

j = M.fromList [('a', (True, 1)), ('b', (False, 2))] ^@.. itraversed <. _1

-- [('a', True), ('b', False)]

k =
  [ M.fromList [("Tulips", 5), ("Roses", 3)],
    M.fromList [("Goldfish", 11), ("Frogs", 8)]
  ]
    ^@.. itraversed
    <.> itraversed

-- [ ((0,"Roses"), 3)
-- , ((0,"Tulips"), 5)
-- , ((1,"Frogs"), 8)
-- , ((1,"Goldfish"), 11)
-- ]

l = [10, 20, 30] & itraversed %@~ (+)

-- [10, 21, 32]

m =
  itraverseOf_
    itraversed
    (\i s -> putStrLn (replicate i ' ' <> s))
    ["one", "two", "three"]

-- one
--  two
--   three

n = ['a' .. 'z'] ^.. itraversed . indices even

-- Exercises - Index Filters

exercises :: M.Map String (M.Map String Int)
exercises =
  M.fromList
    [ ( "Monday",
        M.fromList [("pushups", 10), ("crunches", 20)]
      ),
      ("Wednesday", M.fromList [("pushups", 15), ("handstands", 3)]),
      ( "Friday",
        M.fromList [("crunches", 25), ("handstands", 5)]
      )
    ]

-- Compute the total number of "crunches" you should do this week
o = sum $ exercises ^.. traversed . itraversed . index "crunches"

-- Compute the number of reps you need to do accross all exercises on Wednesday
p = sum $ exercises ^.. itraversed . index "Wednesday" . traversed

-- List out the number of pushups you need to do each day
q = exercises ^@.. itraversed <. itraversed . index "pushups"

board =
  [ "XOO",
    ".XO",
    "X.."
  ]

-- Generate a list of positions alongside their (row, column) coordinates
r = board ^@.. itraversed <.> itraversed

-- Set the empty square at (1, 0) to an 'X'
s = board & (itraversed <.> itraversed) . index (1, 0) .~ 'X'

-- Get the 2nd column as a list. Try to do it using index instead of indices
t = board ^.. itraversed <. traversed . index 1

-- Get the 3rd row as a list. Use index instead of indices
u = board ^.. itraversed . index 2 . traversed

data Board a
  = Board
      a
      a
      a
      a
      a
      a
      a
      a
      a
  deriving (Show, Foldable)

data Position = I | II | III deriving (Show, Eq, Ord)

testBoard ::
  Board Char
testBoard =
  Board
    'X'
    'O'
    'X'
    '.'
    'X'
    'O'
    '.'
    'O'
    'X'

slotsFold :: IndexedFold (Position, Position) (Board a) a
slotsFold =
  ifolding $ \board ->
    zip [(x, y) | y <- [I, II, III], x <- [I, II, III]] (toList board)

v = testBoard ^@.. slotsFold . indices ((== II) . snd)

slotsTraversal ::
  IndexedTraversal (Position, Position) (Board a) (Board b) a b
slotsTraversal
  p
  ( Board
      a1
      b1
      c1
      a2
      b2
      c2
      a3
      b3
      c3
    ) =
    Board <$> indexed p (I, I) a1
      <*> indexed p (II, I) b1
      <*> indexed p (III, I) c1
      <*> indexed p (I, II) a2
      <*> indexed p (II, II) b2
      <*> indexed p (III, II) c2
      <*> indexed p (I, III) a3
      <*> indexed p (II, III) b3
      <*> indexed p (III, III) c3

w = testBoard & slotsTraversal . index (II, II) .~ 'X'

printBoard = itraverseOf_ slotsTraversal printSlot
  where
    printSlot (III, _) c = putStrLn [c]
    printSlot _ c = putStr [c]

x = T.pack "hello" ^@.. indexing each

y = "hello" ^@.. itraversed

z = ['a' .. 'c'] ^@.. reindexed show itraversed

-- [("0",'a'),("1",'b'),("2",'c')]

-- Exercises - Custom Indexed Optics

-- Write a sensible implementation for the following indexed fold

pair :: IndexedFold Bool (a, a) a
pair = ifolding $ \(x, y) -> [(False, x), (True, y)]

a' = ('a', 'b') ^@.. pair

-- [(False,'a'),(True,'b')]

pairTraversal :: IndexedTraversal Bool (a, a) (b, b) a b
pairTraversal b (x, y) = (,) <$> indexed b False x <*> indexed b True y

b' = ('a', 'b') & pairTraversal . index True .~ 'c'

oneIndexed :: IndexedTraversal Int [a] [b] a b
oneIndexed = reindexed (+ 1) itraversed

c' = ['a' .. 'd'] ^@.. oneIndexed

invertedIndexed :: IndexedTraversal Int [a] [b] a b
invertedIndexed =
  reindexed
    (\(xs, i) -> (length xs - 1) - i)
    (selfIndex <.> itraversed)

-- Build the following combinators using only compositions of other optics
chars :: IndexedTraversal Int T.Text T.Text Char Char
chars = indexing each

charCoords :: IndexedTraversal (Int, Int) String String Char Char
charCoords = indexing lined <.> indexing each

_1Ipl :: Field1 s t a b => IndexPreservingLens s t a b
_1Ipl = cloneIndexPreservingLens _1

d' = [('a', True), ('b', False), ('c', True)] ^@.. itraversed . _1Ipl
