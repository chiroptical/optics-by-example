{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Lens hiding (List)
import Data.Char
import Data.Data
import Data.Data.Lens
import Data.List
import qualified Data.Map as M
import Data.Tree
import Data.Tree.Lens

data Expr
  = Val Int
  | Expr :+: Expr
  | Expr :*: Expr
  deriving (Show, Data, Eq)

deriving instance Plated Expr

a = children [1, 2, 3]

-- [[2, 3]]

b = children ((Val 1 :+: (Val 2 :*: Val 3)) :*: Val 4)

unDup :: Eq a => [a] -> Maybe [a]
unDup (x : y : xs) | x == y = Just (x : xs)
unDup _ = Nothing

c = rewrite unDup [1, 2, 2, 2, 3, 2, 4]

simplifyAST :: Expr -> Maybe Expr
simplifyAST (Val 0 :+: e) = Just e
simplifyAST (e :+: Val 0) = Just e
simplifyAST (Val 1 :*: e) = Just e
simplifyAST (e :*: Val 1) = Just e
simplifyAST _ = Nothing

d = rewrite simplifyAST ((Val 1 :*: Val 10) :+: Val 0)

-- transform is similar to rewrite except it works from bottom to top and only
-- applies the transform once. rewrite could potentially never terminate

treasureTree :: Tree (M.Map String String)
treasureTree =
  Node
    (M.singleton "junk" "ignored")
    [ Node
        (M.singleton "treasure" "gems")
        [Node (M.singleton "treasure" "rubies") []],
      Node (M.fromList [("treasure", "gold")]) []
    ]

e = treasureTree ^.. deep (root . ix "treasure")

-- ["gems", "gold"] -- but not "rubies"

-- To get all children with ix "treasure" we need cosmos
f = treasureTree ^.. cosmos . root . ix "treasure"

searching :: (a -> Bool) -> Traversal' (Tree a) (Tree a)
searching predicate = branches . traversed . filtered (predicate . rootLabel)

numberTree :: Tree Int
numberTree =
  Node
    10
    [ Node
        4
        [Node 2 [], Node 3 []],
      Node
        7
        [Node 8 [], Node 3 []]
    ]

g = numberTree ^.. cosmosOf (searching (> 5)) . root

-- List all children reachable through paths of odd children
-- Note that the root isn't filtered, we only run the predicate on *children*
h = numberTree ^.. cosmosOf (searching odd) . root

-- [10,7,3]

i = transformOf (searching odd) (root %~ negate) numberTree

data Employee = Employee
  { _name :: String,
    _salary :: Int
  }
  deriving (Show, Data)

data Company = Company
  { _humanResources :: [Employee],
    _salesPeople :: [Employee],
    _managers :: [Employee]
  }
  deriving (Show, Data)

makeLenses ''Employee
makeLenses ''Company

dunderMifflin :: Company
dunderMifflin =
  Company
    { _humanResources = [Employee "Holly Flax" 40000],
      _salesPeople = [Employee "Dwight Schrute" 60000],
      _managers = [Employee "Michael Scott" 80000]
    }

updateSalaryBoring :: Company -> Company
updateSalaryBoring c =
  c
    & humanResources
      . traversed
      . salary
      +~ 1000
    & salesPeople
      . traversed
      . salary
      +~ 1000
    & managers
      . traversed
      . salary
      +~ 1000

j = dunderMifflin & biplate +~ (1000 :: Int)

-- Exercises – Uniplate

data JSON
  = Str String
  | Number Int
  | List [JSON]
  | Obj (M.Map String JSON)
  deriving (Show)

makePrisms ''JSON

-- 1. Implement `plate` for `JSON`

instance Plated JSON where
  plate _ (Str s) = pure $ Str s
  plate _ (Number n) = pure $ Number n
  plate handler (List xs) = List <$> traverse handler xs
  plate handler (Obj xs) = Obj <$> traverse handler xs

-- 2. Write rewrite rule to flatten `Obj`s to `List`s
flattenObjs :: JSON -> Maybe JSON
flattenObjs (Obj m) = Just . List $ M.elems m
flattenObjs _ = Nothing

jsonObj :: JSON
jsonObj =
  Obj
    ( M.fromList
        [ ("quux", Number 1),
          ("flaarg", List [Str "tweedle", Str "twiddle"])
        ]
    )

k = rewrite flattenObjs jsonObj

-- 3. Use cosmos to get a list of all strings
l = jsonObj ^.. cosmos . _Str

-- 4. Use deep to reverse all strings in JSON structure
-- - Should this really be `transform f jsonObj`?
m = jsonObj & deep _Str %~ reverse

-- 5. Use biplate to get a list of all characters in the following structure
-- then use biplate to uppercase them all

structure :: (String, (Bool, String), [Either String String])
structure = ("Gryffindor", (True, "Ravenclaw"), [Left "Slytherin", Right "Hufflepuff"])

type Structure = (String, (Bool, String), [Either String String])

-- Note. Type inference with biplate can be an interesting beast check out
-- these examples below where the types of biplate influence the result

n = structure ^.. biplate @Structure @String . traversed

o = structure ^.. biplate @Structure @Char

p :: Structure
p =
  structure
    & biplate @Structure @Char
    %~ toUpper

q :: Structure
q =
  structure
    & biplate @Structure @String
    %~ fmap toUpper

r :: Structure
r =
  structure
    & biplate @Structure
    %~ fmap @[] toUpper
