{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens
import Data.List (stripPrefix)
import Data.List.Split (chunksOf)
import Data.Monoid (Sum (Sum))

a = Left "message" ^? _Left

b = Left "message" ^? _Right

-- This compiles, but it is awkward...
c = Nothing & _Nothing %~ const ()

d = has _Right (Left "msg")

e = isn't _Nothing (Just 1)

newtype Path = Path
  { _path :: [String]
  }
  deriving (Show)

newtype Body = Body
  { _body :: String
  }
  deriving (Show)

makeLenses ''Path
makeLenses ''Body

data Request
  = Post Path Body
  | Get Path
  | Delete Path
  deriving (Show)

makePrisms ''Request

f = Get (Path ["users"]) ^? _Get . path

g = _Get . path # ["..."]

h = _Post # (path # ["..."], body # "...")

i = _Cons # (1, [2, 3])

j = (Sum <$> [1, 2, 3]) ^. _Cons

k = ([] :: [Sum Integer]) ^. _Cons

l = "Hello" & _head .~ 'J'

m = _Show # 1

n :: Maybe Integer
n = "1" ^? _Show

o :: [Int]
o = "It's True that I ate 3 apples and 5 oranges" ^.. worded . _Show

-- 1. Which Prisms will be created for the following code

data ContactInfo
  = Email String
  | Telephone Int
  | Address String String String

makePrisms ''ContactInfo

-- _Email :: Prism' ContactInfo String
-- _Telephone :: Prism' ContactInfo Int
-- _Address :: Prism' ContactInfo (String, String, String)

-- 2. Fill in the blanks

p = Right 35 & _Right +~ 5

-- Right 40

q = [Just "Mind", Just "Power", Nothing] ^.. folded . _Just

-- ["Mind", "Power"]

r = [Just "Mind", Just "Power", Nothing] & traversed . _Just <>~ " Stone"

-- [Just "Mind Stone", Just "Power Stone", Nothing]

s = Left (Right True, "Eureka!") & _Left . _1 . _Right %~ not

-- Left (Right False, "Eureka!")

t = _Cons `review` ("Do", ["Re", "Mi"])

-- ["Do", "Re", "Mi"]

u = isn't (_Show :: Prism' String Int) "not an int"

-- True

-- 3. Write an expression to convert input to output

v =
  let input = (Just 1, Nothing, Just 3)
      output = [1, 3]
   in input ^.. each . _Just

w =
  let input = ('x', "yz")
      output = "xzy"
   in _Cons # input & _tail %~ reverse

x =
  let input = "do the hokey pokey"
      output = Left (Just (Right "do the hokey pokey"))
   in _Left . _Just . _Right # input

data A = A String | B String

_A :: Prism' A String
_A = prism' A $ \case
  A s -> Just s
  _ -> Nothing

_Just' :: Prism (Maybe a) (Maybe b) a b
_Just' = prism Just $ \case
  Just a -> Right a
  _ -> Left Nothing

_Prefix :: String -> Prism' String String
_Prefix prefix = prism' (prefix <>) $ \case
  s -> stripPrefix prefix s

y = _Prefix "hello " # "world"

z = "hello world" ^? _Prefix "hello"

_Factor :: Int -> Prism' Int Int
_Factor factor = prism' (factor *) $ \case
  n | n `mod` factor == 0 -> Just (n `div` factor)
  _ -> Nothing

-- Exercises

-- 1. Try to write a custom prism for matching on the tail of a list

_Tail :: Prism' [a] [a]
_Tail = prism' embed match
  where
    embed :: [a] -> [a]
    embed = id
    match :: [a] -> Maybe [a]
    match = \case
      (_ : t) -> Just t
      _ -> Nothing

-- This is incorrect because you can't _embed_ the tail of an empty list and
-- embed must succeed

-- 2. Implement _Cons for lists

_Cons' :: forall a b. Prism [a] [b] (a, [a]) (b, [b])
_Cons' = prism embed match
  where
    embed :: (b, [b]) -> [b]
    embed (h, t) = h : t
    match :: [a] -> Either [b] (a, [a])
    match = \case
      (h : t) -> Right (h, t)
      _ -> Left []

-- 3. Implement _Cycles which detects `n` repetitions of patterns

_Cycles :: forall a. Eq a => Int -> Prism' [a] [a]
_Cycles n = prism' embed match
  where
    embed :: [a] -> [a]
    embed = concat . replicate n -- Or, go n xs
    -- go n xs
    --   | n <= 0 = []
    --   | otherwise = xs <> go (n - 1) xs

    -- Alternatively, we could take the "cycle" from the front of the list and
    -- then replicate it and check if they are equal.
    --
    -- TODO: Can we use (#) to materialize the correct cycle and compare it to
    -- the given cycle
    match :: [a] -> Maybe [a]
    match xs =
      let chunks = chunksOf itemsPerChunk xs
          isCorrectLength = length chunks == n
          itemsPerChunk = length xs `div` n
       in if n == 0 || not isCorrectLength
            then Nothing
            else case chunks of
              [] -> Nothing
              (h : t) ->
                if all (h ==) t
                  then Just h
                  else Nothing
