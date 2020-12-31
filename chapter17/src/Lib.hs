{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Lib where

import Control.Lens
import Data.Generics.Product
import Data.Generics.Sum
import GHC.Generics

data Person = Person
  { name :: String,
    age :: Int
  }
  deriving (Show, Generic)

jon :: Person
jon = Person "Jon" 32

a = jon ^. field @"name"

data Person' = Person' String Int
  deriving (Show, Generic)

jon' :: Person'
jon' = Person' "Jon" 32

b = jon' ^. position @1

c = jon' ^. typed @String

-- typed will generate a compiler error if you have two String fields

d = jon ^. the @"name"

e = jon ^. the @Int

f = jon ^. the @1

data Dwelling = Dwelling
  { address :: String,
    numBedrooms :: Int
  }
  deriving (Show, Generic)

data Apartment = Apartment
  { address :: String,
    numBedrooms :: Int,
    allowsPets :: Bool
  }
  deriving (Show, Generic)

data House = House
  { address :: String,
    numBedrooms :: Int,
    lotSize :: Int
  }
  deriving (Show, Generic)

apt = Apartment "2020 Willow St. Apt 307" 2 True

dwelling = Dwelling "2020 Willow St. Apt 409" 3

g = apt ^. super @Dwelling

h = apt & super .~ dwelling

data IntPair = IntPair Int Int
  deriving (Show, Generic)

i = IntPair 1 2 ^.. types @Int

data ListTuple a b = ListTuple a [b]
  deriving (Show, Generic)

tup = ListTuple "a" ["b1", "b2"]

j = tup ^.. param @0

-- ["b1", "b2"]

k = tup ^.. param @1

-- ["a"]

data Video
  = DVD String
  | VHS String
  | LaserDisc String
  deriving (Show, Generic)

l = DVD "The Princess Bride" ^? _Ctor @"DVD"

m = VHS "The Princess Bride" ^? _Ctor @"DVD"

n = _Ctor @"LaserDisc" @Video # "King Kong"

data ID
  = NumID Int
  | StrID String
  deriving (Show, Generic)

o = NumID 3 ^? _Typed @Int

-- (Left "ambiguity" :: Either String String) ^? _Typed @String
-- would yield a type error because which String should the prism choose

p = (Left "ambiguity" :: Either String Int) ^? _Typed @String

-- Exercises

-- 1.

newtype Email = Email
  { email :: String
  }
  deriving (Show, Generic)

validateEmailField :: HasField' "email" s String => s -> Bool
validateEmailField = elemOf (field' @"email" . traversed) '@'

q = validateEmailField (Email "okay@gmail.com")

r = validateEmailField (Email "not okay")

-- 2.

setFirstSimple :: HasPosition' 1 s a => s -> a -> s
setFirstSimple s a = s & position' @1 .~ a

s = setFirstSimple (Person "chiro" 32) "chiroptical"

setFirstComplex :: HasPosition 1 s t a b => s -> b -> t
setFirstComplex s b = s & position @1 .~ b

t = setFirstComplex (ListTuple 0 [0, 0]) "hello"

setComplex :: forall n s t a b. HasPosition n s t a b => s -> b -> t
setComplex s b = s & position @n .~ b

u = setComplex @2 (ListTuple 0 [0, 0]) ["hello", "world"]

-- 3.

getMember :: forall s a. HasType a s => s -> a
getMember s = s ^. typed @a @s

type Structure = ListTuple Integer String

v = getMember @Structure @([] String) (ListTuple 0 ["hello"])

-- 4.
-- Note. had to look this one up, doesn't really make sense to me

validateEmail :: Subtype Email sub => sub -> Bool
validateEmail sub = case sub ^. super @Email of
  Email e -> '@' `elem` e

-- 5.

-- Tried this, but it doesn't compile. Unsure if you can do something
-- like this...
-- gmap :: forall f s t a b. (Functor f, HasType (f a) s) => (a -> b) -> s -> t
-- gmap g s = s ^. typed @(f a) %~ g

gmap :: HasParam 0 s t a b => (a -> b) -> s -> t
gmap f s = s & param @0 %~ f

-- 6.

intTypedPrism :: (AsType Int s, Choice p, Applicative f) => p Int (f Int) -> p s (f s)
intTypedPrism = _Typed @Int

w = intTypedPrism # 42 :: Maybe Int

x = (Left 13 :: Either Int String) ^? intTypedPrism
