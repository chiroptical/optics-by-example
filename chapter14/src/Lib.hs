{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader

newtype Person = Person
  { _personName :: String
  }
  deriving (Show)

newtype Pet = Pet
  { _petName :: String
  }
  deriving (Show)

makeFields ''Person
makeFields ''Pet

greetByName :: HasName r String => r -> IO ()
greetByName r = putStrLn $ "Hello " <> r ^. name <> "!"

data Env = Env
  { _portNumber :: Int,
    _hostName :: String,
    _databaseUrl :: String
  }
  deriving (Show)

makeLenses ''Env

connectDb :: (MonadIO m, MonadReader Env m) => m ()
connectDb = do
  url <- view databaseUrl
  liftIO $ putStrLn ("connecting to db at: " <> url)

initialize :: (MonadIO m, MonadReader Env m) => m ()
initialize = do
  port <- view portNumber
  host <- view hostName
  liftIO . putStrLn $ "initializing server at: " <> host <> ":" <> show port

a :: IO ()
a = flip runReaderT (Env 8000 "example.com" "db.example.com") $ do
  initialize
  connectDb
