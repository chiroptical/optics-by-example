{-# LANGUAGE RecordWildCards #-}
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
import           Control.Lens.Unsound           ( lensProduct )
import           Control.Applicative
import           Data.Char
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Text                     as T
import           Data.List                      ( intercalate )

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

-- Commented out for makeLenses function below
-- numCrew :: Lens' Ship Int
-- numCrew = lens _numCrew (\ship num -> ship { _numCrew = num })

-- Exercises: Lenses and records - Records Part One

-- 1. The structure and focus of a lens are represented by which characters:
-- structure: s
-- focus: a

-- 2. Which two components are required to create a lens
-- A getter and a setter

-- 3. Implement the following lens:
-- Commented out for makeLenses function below
-- name :: Lens' Ship String
-- name = lens _name (\ship name -> ship { _name = name })

purplePearl :: Ship
purplePearl = Ship { _name = "Purple Pearl", _numCrew = 38 }

makeLenses ''Ship

-- Exercises: Lenses and records - Records Part Two

-- 1. List the lenses which are generated for the following code

data Wand = Wand
data Book = Book
data Potion = Potion

data Inventory =
  Inventory { _wand :: Wand
            , _book :: Book
            , _potions :: [Potion]
            }

makeLenses ''Inventory

-- wand :: Functor f => (Wand -> f Wand) -> Inventory -> f Inventory
-- book :: Functor f => (Book -> f Book) -> Inventory -> f Inventory
-- potions :: Functor f => ([Potion] -> f [Potion]) -> Inventory -> f Inventory

-- 2. Rewrite the following tpye as `Lens'`

data Spuzz = Spuzz

newtype Chumble =
  Chumble
    { _spuzz :: Spuzz
    }

-- gazork :: Lens' Chumble Spuzz
gazork :: Functor f => (Spuzz -> f Spuzz) -> Chumble -> f Chumble
gazork = undefined

data Pet =
  Pet { _petName :: String
      , _petType :: String
      }

makeLenses ''Pet

getPetName :: Pet -> String
getPetName = view petName

type Conditional a = (Bool, a, a)

conditional :: Lens' (Conditional String) String
conditional = lens getter setter
 where
  getter (True , x, _) = x
  getter (False, _, x) = x
  setter (True , x, y) x' = (True, x', y)
  setter (False, y, x) x' = (True, y, x')

data Err =
    ReallyBadError { _msg :: String }
  | ExitCode { _code :: Int }
  deriving (Eq)

-- makeLenses ''Err

type UserName = String
type UserId = String

data Session =
  Session { _userId :: UserId
          , _userName :: UserName
          , _createdTime :: String
          , _expiryTime :: String
          } deriving (Show, Eq)

makeLenses ''Session

userInfo :: Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName

-- Exercises: Lens Laws - Laws

-- set-get ~ view myLens (set myLens newValue structure) == newValue
-- get-set ~ set myLens (view myLens structure) == structure
-- set-set ~ set myLens differentValue (set myLens value structure)
--            == set myLens differentValue structure

-- 1. Implement a lens which breaks the get-set and/or set-set law

-- Unsure... Took example from Chris

-- recorder carries a "state" and therefore laws containing
-- comparisons of the tuples will fail, i.e. get-set set-set

recorder :: Lens' ([a], a) a
recorder = lens getter setter
 where
  getter (_, a) = a
  setter (as, a) val = (a : as, val)

-- 2. Test the get-set and set-set laws for the `msg` type

msgSetGet = view msg (set msg val structure) == val
 where
  val       = "hello"
  structure = ReallyBadError "world"

msgSetGet' = view msg (set msg val structure) == val
 where
  val       = "hello"
  structure = ExitCode 0

msgGetSet = set msg (view msg structure) structure == structure
  where structure = ReallyBadError "hello"

msgGetSet' = set msg (view msg structure) structure == structure
  where structure = ExitCode 0

msgSetSet = set msg diff (set msg val structure) == set msg diff structure
 where
  diff      = "diff"
  val       = "val"
  structure = ReallyBadError "structure"

msgSetSet' = set msg diff (set msg val structure) == set msg diff structure
 where
  diff      = "diff"
  val       = "val"
  structure = ExitCode 0

-- 3. Rewrite `msg` such that it passes the set-get law and set-set law but not get-set

msg :: Lens' Err String
msg = lens getter setter
 where
  getter (ReallyBadError s) = s
  getter (ExitCode       _) = ""
  setter (  ReallyBadError _) s = ReallyBadError s
  -- fails set-get law
  -- setter e@(ExitCode _) _ = e
  -- fails get-set law
  setter e@(ExitCode       _) s = ReallyBadError s

-- 4. Think up a lens which is still useful but breaks a law or two

-- Unsure...

-- After looking up the answer to "1.", it seems that anything
-- with an internal ``state'' would fail the get-set and set-set laws
-- however state is a useful tool

-- 5. Write a lens which violates all three laws

newtype Y =
  Y { _y :: Int } deriving (Eq)

y :: Lens' Y String
y = lens getter setter
 where
  getter :: Y -> String
  getter (Y x) = show x
  setter :: Y -> String -> Y
  setter (Y x) s = Y (x + length s)

ySetGet = view y (set y newValue structure) == newValue
 where
  newValue  = "abc"
  structure = Y 10

yGetSet = set y (view y structure) structure == structure
  where structure = Y 10

ySetSet = set y diff (set y val structure) == set y diff structure
 where
  diff      = "abc"
  val       = "wxyz"
  structure = Y 10

-- 6. Write a lawful lens for the following type

data Builder =
  Builder { _context :: [String]
          , _build :: [String] -> String
          }

build :: Lens' Builder String
build = lens getter setter
 where
  getter Builder {..} = _build _context
  setter Builder {..} s = Builder { _context = _context, _build = const s }

buildSetGet = view build (set build val struct) == val
 where
  val    = "hello"
  struct = Builder ["foo", "bar", "baz"] concat

applyContext :: Builder -> String
applyContext Builder {..} = _build _context

buildGetSet = applyContext (set build (view build struct) struct)
  == applyContext struct
  where struct = Builder ["foo", "bar", "baz"] concat

buildSetSet = applyContext (set build diff (set build val struct))
  == applyContext (set build diff struct)
 where
  diff   = "hello"
  val    = "world"
  struct = Builder ["foo", "bar", "baz"] concat

-- Section 3.6 Virtual Fields

data Temperature =
  Temperature { _location :: String
              , _celsius :: Float
              } deriving Show

makeLenses ''Temperature

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5 / 9)

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = c * (9 / 5) + 32

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
 where
  getter = celsiusToFahrenheit . view celsius
  setter temp f = set celsius (fahrenheitToCelsius f) temp

data User =
  User { _firstName :: String
       , _lastName :: String
       , _email :: String
       } deriving Show

makeLenses ''User

username :: Lens' User String
username = lens getter setter
 where
  getter = view email
  setter user name = set email name user

fullName :: Lens' User String
fullName = lens getter setter
 where
  getter user = view firstName user ++ " " ++ view lastName user
  setter user name =
    let (h : t) = words name in user { _firstName = h, _lastName = unwords t }

-- 3.7 Data correction and maintaining invariants

data Time =
  Time { _hours :: Int
       , _mins :: Int
       } deriving Show

clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal = min maxVal . max minVal

-- Naive lenses
-- hours :: Lens' Time Int
-- hours = lens getter setter
--   where
--     getter (Time h _) = h
--     setter (Time _ m) newHours = Time (clamp 0 23 newHours) m

-- mins :: Lens' Time Int
-- mins = lens getter setter
--   where
--     getter (Time _ m) = m
--     setter (Time h _) newMins = Time h (clamp 0 59 newMins)

-- Including rollover
hours :: Lens' Time Int
hours = lens getter setter
  where
    getter (Time h _) = h
    setter (Time _ m) newHours = Time (newHours `mod` 24) m

mins :: Lens' Time Int
mins = lens getter setter
  where
    getter (Time _ m) = m
    setter (Time h _) newMins =
      Time ((h + (newMins `div` 60)) `mod` 24) (newMins `mod` 60)

-- Exercises - Self-Correcting Lenses

data ProducePrices =
  ProducePrices { _limePrice :: Float
                , _lemonPrice :: Float
                } deriving Show

-- setters which avoid negative values

limePrice' :: Lens' ProducePrices Float
limePrice' = lens getter setter
  where
    getter = _limePrice
    setter pps newPrice
      | newPrice > 0 = pps { _limePrice = newPrice }
      | otherwise = pps { _limePrice = 0 }

lemonPrice' :: Lens' ProducePrices Float
lemonPrice' = lens getter setter
  where
    getter = _lemonPrice
    setter pps newPrice
      | newPrice > 0 = pps { _lemonPrice = newPrice }
      | otherwise = pps { _lemonPrice = 0 }

-- setters which avoid changes of greater than 50 cents

limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
  where
    getter = _limePrice
    setter pps newPrice =
      let
        price = _limePrice pps
        clamp' = clamp (price - 0.5) (price + 0.5)
      in
        pps { _limePrice = clamp' newPrice }

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getter setter
  where
    getter = _lemonPrice
    setter pps newPrice =
      let
        price = _lemonPrice pps
        clamp' = clamp (price - 0.5) (price + 0.5)
      in
        pps { _lemonPrice = clamp' newPrice }
