{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens

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
a = (payload . weightKilos) %~ show
