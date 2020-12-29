{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable
import qualified Data.Map as M
import Text.Printf

newtype Username = Username
  { _username :: String
  }
  deriving newtype (Show, Eq, Ord)

newtype Password = Password
  { _password :: String
  }
  deriving newtype (Show, Eq, Ord)

data Env = Env
  { _currentUser :: Username,
    _users :: M.Map Username Password
  }
  deriving (Show)

makeLenses ''Username
makeLenses ''Password
makeLenses ''Env

printUser :: ReaderT Env IO ()
printUser = do
  user <- view (currentUser . username)
  liftIO . putStrLn $ "Current user: " <> user

getUserPassword :: ReaderT Env IO ()
getUserPassword = do
  user <- view currentUser
  mPassword <- preview (users . ix user)
  liftIO $ print mPassword

a :: IO ()
a = do
  let user = username # "jenkins"
      pass = password # "hunter2"
  runReaderT printUser (Env user (M.singleton user pass))

b :: IO ()
b = do
  let user = username # "jenkins"
      pass = password # "hunter2"
  runReaderT getUserPassword (Env user (M.singleton user pass))

data Till = Till
  { _total :: Double,
    _sales :: [Double],
    _taxRate :: Double
  }
  deriving (Show)

makeLenses ''Till

tuesdaySales :: StateT Till IO ()
tuesdaySales = do
  sales <>= pure 8.55
  sales <>= pure 7.36
  taxRate' <- use taxRate
  salesWithTax <- (taxRate' *) <$> use (sales . to sum)
  total .= salesWithTax
  totalSales <- use total
  liftIO $ printf "Total sale: $%.2f\n" totalSales

c :: IO ()
c = execStateT tuesdaySales (Till 0 [] 1.11) >>= print

data Weather = Weather
  { _temperature :: Float,
    _pressure :: Float
  }
  deriving (Show)

makeLenses ''Weather

printData :: String -> ReaderT Float IO ()
printData statName = do
  num <- ask
  liftIO . putStrLn $ statName <> ": " <> show num

weatherStats :: ReaderT Weather IO ()
weatherStats = do
  magnify temperature (printData "temperature")
  magnify pressure (printData "pressure")

d :: IO ()
d = runReaderT weatherStats (Weather 15 7.2)

convertCelsiusToFahrenheit :: StateT Float IO ()
convertCelsiusToFahrenheit = do
  modify (\celsius -> (celsius * (9 / 5)) + 32)

weatherStatsFahrenheit :: StateT Weather IO ()
weatherStatsFahrenheit = zoom temperature convertCelsiusToFahrenheit

e :: IO ()
e = execStateT weatherStatsFahrenheit (Weather 32 12) >>= print
