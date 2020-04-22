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

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Either                    ( lefts )
import Numeric.Lens (negated)
import Data.Data.Lens (biplate)
import Data.List (sort)
import Data.Bits.Lens (bitAt)

newtype Address =
  Address
    { country :: String
    }

newtype Person =
  Person
    { address :: Address
    }

viewCountry :: Person -> String
-- viewCountry Person { address = Address { country = x } } = x
viewCountry = country . address

-- View a nested field of some record `Person`
-- view (address . country) person

setThree :: (a, b, c) -> Bool -> (a, b, Bool)
-- setThree t b = const b <$> t
setThree = flip (<$)

-- set _3 False ('a', 'b', 'c')
-- ~ ('a', 'b', False)

list :: [(Bool, Either Int String)]
list = [(True, Left 10), (False, Right "pepperoni"), (True, Left 20)]

-- sumOfLefts :: [(Bool, Either Int String)] -> Int
-- sumOfLefts = foldr f 0 . fmap snd
--   where
--     f (Left x) acc = acc + x
--     f (Right _) acc = acc

sumOfLefts :: [(Bool, Either Int String)] -> Int
sumOfLefts = sum . lefts . fmap snd

-- sumOf (folded . _2 . _Left) [(True, Left 10), (False, Right "pepperoni"), (True, Left 20)]
-- ~ 30

stories = ["This one time at band camp", "Nuff said.", "This is a short story"]

truncateStrings :: [String] -> [String]
truncateStrings = fmap (\s -> if length s > 10 then take 10 s ++ "..." else s)

-- let stories = ["This one time at band camp", "Nuff said.", "This is a short story"]
--  over
--    (traversed . filtered ((>10) . length))
--    (\story -> take 10 story ++ "...")
-- ~ ["This one t...", "Nuff said.", "This is a ..."]

eitherList :: [Either Int Int]
eitherList = [Left 1, Right 10, Left 2, Right 20]

sumWithEither :: [Either Int Int] -> Int
sumWithEither = sum . fmap (either negate id)

sumWithOptic :: [Either Int Int] -> Int
sumWithOptic = sumOf (folded . beside negated id)
-- `beside` applies the function to the `Left` (`negated` here) and to the `Right`
-- (`id` here). They are folded to `[-1, 10, -2, 30]` and summed up.

thing :: (Maybe Int, Either (String, [Int]) String)
thing = (Just 3, Left ("hello", [13, 15, 17])) & biplate *~ (100 :: Int)
-- `biplate` is a wild function. It will find any `Int` and multiply by 100

thing' :: [Integer]
thing' = [1, 2, 3, 4, 5] & partsOf (traversed . filtered even) %~ reverse
-- This reverse sorts the even numbers in place

thing'' :: (String, String, String)
thing'' = ("one", "two", "three") & partsOf (each . traversed) %~ sort
-- This sorts the characters in place but preserves the structure

thing_ :: [Int]
thing_ = [1, 2, 3, 4] & traversed . bitAt 1 %~ not

prompts = ( "What is your name?"
          , "What is your quest?"
          , "What is your favourite color?"
          )

thing__ :: IO (String, String, String)
thing__ = prompts & each %%~ (\prompt -> putStrLn prompt >> getLine)
