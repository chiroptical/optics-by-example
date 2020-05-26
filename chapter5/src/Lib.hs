{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           Control.Lens
import           Data.Char                      ( toUpper )

-- 5.1 Lens Operators

data Payload a =
  Payload
    { _weightKilos :: a
    , _cargo :: String
    } deriving Show

makeLenses ''Payload

newtype Ship a =
  Ship
    { _payload :: Payload a
    } deriving Show

makeLenses ''Ship

serenity :: Ship Int
serenity = Ship (Payload 50000 "Livestock")

-- In the book, the `Payload` `_weightKilos` field is fixed to `Int` If we want
-- `a` to compile however the structure must be allowed to vary. This means
-- that I needed to modify `Ship` and `Payload` to be polymorphic.

a :: Ship Int -> Ship String
a = payload . weightKilos %~ show

b :: Ship a -> Ship String
b = payload . weightKilos .~ "10000"

c :: Ship a -> a
c = (^. payload . weightKilos)

d :: Ship Int
d = serenity & payload . weightKilos %~ (+ 10000)

newtype Thermometer =
  Thermometer
    { _temperature :: Int
    } deriving Show

makeLenses ''Thermometer

e :: (Int, Thermometer)
e = Thermometer 20 & temperature <+~ 15
-- Result: (35, Thermometer 35)

f :: (Int, Thermometer)
f = Thermometer 20 & temperature <<+~ 15
-- Result: (20, Thermometer 35)

-- Exercises -- Operators

-- 1. Using the following types, lenses, and infix operators generate the
--    final states in book (pgs 73-74)

data Gate =
  Gate
    { _open :: Bool
    , _oilTemp :: Float
    } deriving Show

makeLenses ''Gate

data Army =
  Army
    { _archers :: Int
    , _knights :: Int
    } deriving Show

makeLenses ''Army

data Kingdom =
  Kingdom
    { _name :: String
    , _army :: Army
    , _gate :: Gate
    } deriving Show

makeLenses ''Kingdom

duloc :: Kingdom
duloc =
  Kingdom { _name = "Duloc"
          , _army = Army { _archers = 22
                         , _knights = 14
                         }
          , _gate = Gate { _open = True
                        , _oilTemp = 10.0
                        }
          }

goalA :: Kingdom
goalA = duloc
  & name <>~ ": a perfect place"
  & army . knights +~ 28
  & gate . open %~ not


goalB :: Kingdom
goalB = duloc
  & name <>~ "instein"
  & army . archers .~ 17
  & army . knights .~ 26
  & gate . oilTemp *~ 10

goalC :: (String, Kingdom)
goalC =
  let
    (r, s) = duloc & name <<>~ ": Home"
  in
    (r, s
      & name <>~ " of the talking Donkeys"
      & gate . oilTemp //~ 2
    )

goalC' :: (String, Kingdom)
goalC' =
  duloc
    & name <<>~ ": Home"
    & _2 . name <>~ " of the talking Donkeys"
    & _2 . gate . oilTemp //~ 2

-- 2. Fill in the blanks

g :: (Bool, String)
g = (False, "opossums") & _1 ||~ True

h :: Int
h = 2 & id *~ 3

i :: ((Bool, String), Float)
i = ((True, "Dudley"), 55.0)
  & _1 . _2 <>~ " - the worst"
  & _2 -~ 15
  & _2 //~ 2
  & _1 . _2 %~ map toUpper
  & _1 . _1 .~ False

-- 3. Name a lens that takes only two arguments
-- view aka ^.

-- 4. What's the type signature of `(%~)` ?
-- :t (%~)
--
-- Functor f => (a -> f b) -> s -> f t
--
-- Lens s t a b -> (a -> b) -> s -> t
