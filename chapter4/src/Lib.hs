{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens

-- Lens s t a b
-- s: structure before action
-- t: structure after action
-- a: focus before action
-- b: focus after action
_1' :: Lens (a, other) (b, other) a b
_1' = _a

data Promotion a = Promotion
  { _item :: a,
    _discountPercentage :: Double
  }
  deriving (Show)

item :: Lens (Promotion a) (Promotion b) a b
item = lens getter setter
  where
    getter = _item
    setter promo newItem = promo {_item = newItem}

data Preferences a = Preferences
  { _best :: a,
    _worst :: a
  }
  deriving (Show)

-- This will fail to compile because we can't
-- return a `Preferences b` without adjusting the `_worst`
-- data constuctor
-- best :: Lens (Preferences a) (Preferences b) a b
-- best = lens getter setter
--   where
--     getter = _best
--     setter pref newBest = pref { _best = newBest }

-- Exercises - Polymorphic Lenses

-- 1.  write the type of `setter` for Vorpal

newtype Vorpal a = Vorpal a

vorpal :: Lens (Vorpal a) (Vorpal b) a b
vorpal = lens getter setter
  where
    getter (Vorpal x) = x
    setter :: Vorpal a -> b -> Vorpal b
    setter (Vorpal _) = Vorpal

-- 2. Find one possible way to write a polymorphic lens which changes the type
-- of the best and worst fields in the Preferences type above

preferences :: Lens (Preferences a) (Preferences b) (a, a) (b, b)
preferences = lens getter setter
  where
    getter Preferences {_best = best, _worst = worst} = (best, worst)
    setter _ (best, worst) = Preferences best worst

-- 3. Type of lens which can change the type variable below

data Result e = Result
  { _lineNumber :: Int,
    _result :: Either e String
  }

result :: Lens (Result a) (Result b) (Either a String) (Either b String)
result = lens getter setter
  where
    getter :: Result a -> Either a String
    getter = _result

    setter :: Result a -> Either b String -> Result b
    setter result newResult = result {_result = newResult}

-- 4. Is it possible to change more than one type variable at a time using a
--    polymorphic lens?

data Tup a b = Tup
  { fst :: a,
    snd :: b
  }
  deriving (Show)

tup :: Lens (Tup a b) (Tup c d) (Tup a b) (Tup c d)
tup = lens getter setter
  where
    getter = id
    setter _ = id

-- 5. Lens to modify Predicate a to Predicate b

newtype Predicate a = Predicate (a -> Bool)

predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicate = lens getter setter
  where
    getter (Predicate p) = p
    setter _ = Predicate

-- 4.3 Composing Lenses

data Person = Person
  { _name :: String,
    _address :: Address
  }
  deriving (Show)

data Address = Address
  { _streetAddress :: StreetAddress,
    _city :: String,
    _country :: String
  }
  deriving (Show)

data StreetAddress = StreetAddress
  { _streetNumber :: String,
    _streetName :: String
  }
  deriving (Show)

makeLenses ''Person
makeLenses ''Address
makeLenses ''StreetAddress

sherlock :: Person
sherlock =
  Person
    { _name = "S. Holmes",
      _address =
        Address
          { _streetAddress =
              StreetAddress
                { _streetNumber = "221B",
                  _streetName = "Baker Street"
                },
            _city = "London",
            _country = "England"
          }
    }

setStreetNumber :: String -> Person -> Person
setStreetNumber newStreetAddress person =
  let existingAddress = _address person
      existingStreetAddress = _streetAddress existingAddress
   in person
        { _address =
            existingAddress
              { _streetAddress =
                  existingStreetAddress
                    { _streetNumber = newStreetAddress
                    }
              }
        }

setStreetNumber' :: String -> Person -> Person
setStreetNumber' = set (address . streetAddress . streetNumber)

data Player = Player deriving (Show)

data Wool = Wool deriving (Show)

data Sweater = Sweater deriving (Show)

data Item a = Item
  { _material :: a,
    _amount :: Int
  }
  deriving (Show)

makeLenses ''Item

weave :: Wool -> Sweater
weave = const Sweater

gameState :: (Player, Item Wool)
gameState = (Player, Item Wool 5)

a = over (_2 . material) weave gameState

-- Exercises - Lens Composition

-- 1. Fill in the blank with the appropriate composition of tuple lenses in the
-- following statement

b = view (_2 . _1 . _2) ("Ginerva", (("Galileo", "Waldo"), "Malfoy"))

-- 2. Fill in the missing type of mysteryDomino

data Five = Five

data Eight = Eight

data Two = Two

data Three = Three

fiveEightDomino :: Lens' Five Eight
fiveEightDomino = _a

mysteryDomino :: Lens' Eight Two
mysteryDomino = _a

twoThreeDomino :: Lens' Two Three
twoThreeDomino = _a

dominoTrain :: Lens' Five Three
dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino

-- 3. Rewrite `c` below a polymorphic lens

data Armadillo = Armadillo

data Hedgehog = Hedgehog

data Platypus = Platypus

data BabySloth = BabySloth

c :: Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
c = _a

d :: Lens Platypus BabySloth Armadillo Hedgehog
d = _a

-- pre-action structure ~ Platypus
-- post-action structure ~ BabySloth
-- pre-action focus ~ Armadillo
-- post-action focus ~ Hedgehog

-- 4. Try to compose ALL of the following lenses together. What is the type of
-- the Lens?

data Chumble = Chumble

data Spuzz = Spuzz

data Gazork = Gazork

data Trowlg = Trowlg

data Bandersnatch = Bandersnatch

data Yakka = Yakka

data Zink = Zink

data Wattoom = Wattoom

data Grug = Grug

data Pubbawup = Pubbawup

data Foob = Foob

data Mog = Mog

data Boojum = Boojum

data Jabberwock = Jabberwock

data Snark = Snark

data JubJub = JubJub

spuzorktrowmble :: Lens Chumble Spuzz Gazork Trowlg
spuzorktrowmble = _a

gazorlglesnatchka :: Lens Gazork Trowlg Bandersnatch Yakka
gazorlglesnatchka = _a

zinkattumblezz :: Lens Zink Wattoom Chumble Spuzz
zinkattumblezz = _a

gruggazinkoom :: Lens Grug Pubbawup Zink Wattoom
gruggazinkoom = _a

banderyakoobog :: Lens Bandersnatch Yakka Foob Mog
banderyakoobog = _a

boowockugwup :: Lens Boojum Jabberwock Grug Pubbawup
boowockugwup = _a

snajubjumwock :: Lens Snark JubJub Boojum Jabberwock
snajubjumwock = _a

composeLenses :: Lens Snark JubJub Foob Mog
composeLenses =
  snajubjumwock
    . boowockugwup
    . gruggazinkoom
    . zinkattumblezz
    . spuzorktrowmble
    . gazorlglesnatchka
    . banderyakoobog
