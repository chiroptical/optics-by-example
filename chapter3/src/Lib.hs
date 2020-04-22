{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module Lib where

import           Control.Lens
import           Control.Applicative
import           Data.Char
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T

-- Exercises: Introduction to Lenses - Optic Anatomy
-- Identify action, path, structure, and focus

a = view (_1 . _2) ((1, 2), 3)
-- action: view
-- path: _1 . _2
-- structure: ((1, 2), 3)
-- focus: 2

b = set (_2 . _Left) "new" (False, Left "old")
-- action: set
-- path: _2 . _Left
-- structure: (False, Left "old")
-- focus: "old"

c = over (taking 2 worded . traversed) toUpper "testing one to three"
-- action: modify
-- path: taking 2 worded . traversed
-- structure: "testing one to three"
-- focus: "testing one"

d = foldOf (both . each) (["super", "cali"], ["frag", "expi"])
-- action: foldOf
-- path: both . each
-- structure: (["super", "cali"], ["frag", "expi"])
-- focus: "super" "cali" "frag" "expi"

-- Exercises: Lens Actions - Lens Actions

-- Find the structure and the focus of the following lens:

e :: Lens' (Bool, (Int, String)) Int
e = _2 . _1
-- structure: (Bool, (Int, String))
-- focus: Int

-- Write the lens with structure `(Char, Int)` and focus `Char`
f :: Lens' (Char, Int) Char
f = _1

-- Name three actions we can use on Lens
-- 1. view
-- 2. set
-- 3. modify

-- Which lens could I use to focus the chatacter `c` in the structure
g = view _3 ('a', 'b', 'c')

-- Write of the simplified types of each identifier in the statement

h :: (Bool, Int)
h = over _2 (* 10) (False, 2)

data Ship =
  Ship
    { _name :: String
    , _numCrew :: Int
    } deriving Show

numCrew :: Lens' Ship Int
numCrew = lens _numCrew (\ship num -> ship { _numCrew = num })

-- Exercises: Lenses and records - Records Part One

-- 1. The structure and focus of a lens are represented by which characters:
-- structure: s
-- focus: a

-- 2. Which two components are required to create a lens
-- A getter and a setter

-- 3. Implement the following lens:
name :: Lens' Ship String
name = lens _name (\ship name -> ship { _name = name })
