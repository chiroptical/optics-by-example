{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Lib where

import           Control.Lens
import           Control.Applicative            ( ZipList(..) )
import           Control.Monad.Trans.State.Lazy
import qualified Data.Map                      as M
import           Data.Tree
import           Data.Char
import           Data.Either.Validation
import           Text.Read                      ( readMaybe )

-- 7.1 Introduction to Traverals

-- > All lenses are valid traversals but the reverse isn't true

a = ("Bubbles", "Buttercup") & both %~ (++ "!")

b = ("Bubbles", "Buttercup") & both .~ "Blossom"

c = ("Bubbles", "Buttercup") & both %~ length

d = ("Bubbles", "Buttercup", "Blossom") & each %~ length

e = [1 .. 5] & taking 3 traversed *~ 10

f = [1 .. 5] & dropping 3 traversed *~ 10

g =
  "once up a time - optics became mainstream"
    &  takingWhile (/= '-') traversed
    %~ toUpper

h = [1 .. 5] & traversed . filtered even *~ 10

i = ("short", "much longer") & both . filtered ((> 5) . length) %~ reverse

-- 7.2 Traversal Combinators

j = [1 .. 3] & traversed *~ 10

k = ("Batman", "Superman") & traversed %~ take 3

powerLevels = M.fromList
  [ ("Gohan"  , 710)
  , ("Goku"   , 9001)
  , ("Vegeta" , 8000)
  , ("Krillin", 5000)
  , ("Piccolo", 408)
  ]

l = powerLevels & traversed %~ \n -> if n > 9000 then "Over 9000" else show n

opticsTree = Node "Lens" [Node "Fold" [], Node "Traversal" []]

m = opticsTree & traversed %~ reverse

n = "I'll be back!" ^.. worded & traversed .~ "hello"
-- ["hello","hello","hello"]
o = "I'll be back!" & worded .~ "hello"
-- "hello hello hello"

p = "blue suede shoes" & worded . _head %~ toUpper
q = "blue suede shoes" & worded %~ \(x : xs) -> toUpper x : xs

r = ("T-Rex", (42, "Stegosaurus")) & beside id _2 <>~ "!"

s = ([(1, 2), (3, 4)], [5, 6, 7]) & beside (traversed . both) traversed +~ 1

t = ([(1, 2), (3, 4)], [5, 6, 7]) ^.. beside (traversed . both) traversed

u = Left (1, 2) & beside both id %~ negate

v = Right [1, 2] & beside id traversed %~ negate

w = "blue suede shoes" & elementOf worded 1 .~ "leather"

-- 7.3 Traversal Composition

-- Exercises

-- 1. Short answer
-- - What type of optic do you get when compose a traversal with a fold?
-- A fold
--
-- - Which of the optics we've learned can act as a traversal?
-- Lens or a Traversal
--
-- - Which of the optics we've learned can act as a fold?
-- A Lens, Fold, or Traversal

-- 2. Fill in the blank to complete each expression:

x = ("Jurassic", "Park") & both .~ "N/A"
-- ("N/A", "N/A")

y = ("Jurassic", "Park") & both . traversed .~ 'x'
-- ("xxxxxxxx", "xxxx")

z = ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & beside id traversed %~ take 3
-- ("Mal", ["Kay", "Ina", "Jay"])

a' = ("Malcolm", ["Kaylee", "Inara", "Jayne"]) & _2 . element 1 .~ "River"
-- ("Malcolm", ["Kaylee", "River", "Jayne"])

b' =
  ["Die Another Day", "Live and Let Die", "You Only Live Twice"]
    &  traversed
    .  elementOf worded 1
    .  traversed
    .~ 'x'
-- [ "Die xxxxxxx Day"
-- , "Live xxx Let Die"
-- , "You xxxx Live Twice"
-- ]

c' = ((1, 2), (3, 4)) & each . each +~ 1
-- ((2, 3), (4, 5))

d' = (1, (2, [3, 4])) & beside id (beside id traversed) +~ 1
-- (2, (3, [4, 5]))

e' =
  ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
    &  each
    .  filteredBy (_1 . only True)
    .  _2
    .  taking 5 traversed
    %~ toUpper
-- ((True, "STRAWberries"), (False, "Blueberries"), (True, "BLACKberries"))

f' =
  ((True, "Strawberries"), (False, "Blueberries"), (True, "Blackberries"))
    &  each
    %~ snd
-- ("Strawberries", "Blueberries", "Blackberries")

-- 7.4 Traversal Actions

g' :: Maybe (Int, Int)
g' = traverseOf both readMaybe ("1", "2")

h' = traverseOf both (\c -> [toLower c, toUpper c]) ('a', 'b')

i' = traverseOf (both . traversed) (\c -> [toLower c, toUpper c]) ("ab", "cd")

validateEmail :: String -> Validation [String] String
validateEmail email | '@' `elem` email = Success email
                    | otherwise = Failure . pure $ "missing '@': " <> email

emailsCorrect =
  [ ("Mike", "mike@tmnt.io")
  , ("Raph", "raph@tmnt.io")
  , ("Don" , "don@tmnt.io")
  , ("Leo" , "leo@tmnt.io")
  ]

emailsIncorrect =
  [ ("Mike", "mike@tmnt.io")
  , ("Raph", "raph.io")
  , ("Don" , "don.io")
  , ("Leo" , "leo@tmnt.io")
  ]

j' = emailsCorrect & traverseOf (traversed . _2) validateEmail

k' = emailsIncorrect & traverseOf (traversed . _2) validateEmail

l' = sequenceAOf (both . traversed)
                 ([Just "apples", Just "bananas"], [Just "oranges"])

m' :: Maybe (Int, Int)
m' = ("1", "2") & both %%~ readMaybe

n' :: Maybe (Int, Int)
n' = both readMaybe ("1", "2")

-- 7.4 Exercises - Traversal Actions

-- 1. Fill in the blanks, see pg. 146

o' = sequenceAOf _1 (Nothing, "Rosebud")
-- Nothing

p' = sequenceAOf (traversed . _1) [("ab", 1), ("cd", 2)]
-- [ [('a', 1 :: Int) ,('c', 2)]
-- , [('a', 1) ,('d', 2)]
-- , [('b', 1) ,('c', 2)]
-- , [('b', 1) ,('d', 2)]
-- ]

q' = sequenceAOf traversed [ZipList [1, 2], ZipList [3, 4]]
-- ZipList { getZipList = [[1, 3], [2, 4]]

r' =
  sequenceAOf (traversed . _2) [('a', ZipList [1, 2]), ('b', ZipList [3, 4])]
-- ZipList {getZipList = [[('a',1),('b',3)],[('a',2),('b',4)]]}

s' = runState result 0
 where
  result = traverseOf (beside traversed both)
                      (\n -> modify (+ n) >> get)
                      ([1, 1, 1], (1, 1))

-- 2. Rewrite the following using the infix traverseOf i.e. %%~

t' = ("ab", True) & _1 . traversed %%~ \c -> [toLower c, toUpper c]

-- 3. Given the following definitions, write a validation function
-- which uses %%~ to validate that the given user has an age value above
-- zero and below 150

data User =
  User
    { _name :: String
    , _age :: Int
    } deriving Show
makeLenses ''User

data Account =
  Account
    { _id :: String
    , _user :: User
    } deriving Show
makeLenses ''Account

validAccount = Account "hello" $ User "aodhneine" 25
invalidAccount = Account "world" $ User "herrhotzenplotz" 151

validateAge :: Account -> Either String Account
validateAge = user . age %%~ \case
  n | n > 0 && n < 150 -> Right n
  n                    -> Left $ "Age is not possible: " <> show n

-- 7.5 Custom traversals

values :: Applicative f => (a -> f b) -> [a] -> f [b]
values f = foldr (\x acc -> (:) <$> f x <*> acc) (pure [])

u' = ["one", "two", "three"] ^.. values

data Transaction =
    Withdrawal { _amount :: Int }
  | Deposit    { _amount :: Int }
  deriving Show
makeLenses ''Transaction

newtype BankAccount =
  BankAccount
    { _transactions :: [Transaction]
    } deriving Show
makeLenses ''BankAccount

aliceAccount = BankAccount [Deposit 100, Withdrawal 20, Withdrawal 10]

v' = aliceAccount ^.. transactions . traversed

--deposits :: Traversal' [Transaction] Int
--deposits :: Traversal [Transaction] [Transaction] Int Int
deposits :: Applicative f => (Int -> f Int) -> [Transaction] -> f [Transaction]
deposits f [] = pure []
deposits handler (Withdrawal amt : trans) =
  (:) <$> pure (Withdrawal amt) <*> deposits handler trans
deposits handler (Deposit amt : trans) =
  (:) <$> (Deposit <$> handler amt) <*> deposits handler trans

w' = aliceAccount ^.. transactions . deposits

isDeposit :: Transaction -> Bool
isDeposit (Deposit _) = True
isDeposit _           = False

x' = aliceAccount ^.. transactions . traversed . filtered isDeposit . amount

-- Exercises - Custom Traversals

-- 1. Rewrite the amount transaction lens manually using the following traversal

amountT :: Traversal' Transaction Int
-- amountT :: Traversal Transaction Transaction Int Int
-- amountT :: Applicative f => (Int -> f Int) -> Transaction -> f Transaction
amountT f (Withdrawal amt) = Withdrawal <$> f amt
amountT f (Deposit    amt) = Deposit <$> f amt

-- 2. Rewrite the both traversal over tuples

both' :: Traversal (a, a) (b, b) a b
-- both' :: Applicative f => (a -> f b) -> (a, a) -> f (b, b)
both' f (x, y) = (,) <$> f x <*> f y

-- 3. Write a traversal which reflects the change of balance to an account

y' :: Traversal' Transaction Int
-- y' :: Applicative f => (Int -> f Int) -> Transaction -> f Transaction
y' f (Withdrawal amt) = Withdrawal . negate <$> f (negate amt)
y' f (Deposit    amt) = Deposit <$> f amt

-- 4. Implement left

left :: Traversal (Either a b) (Either a' b) a a'
--left :: Applicative f => (a -> f a') -> Either a b -> f (Either a' b)
left f (Left  x) = Left <$> f x
left _ (Right x) = pure $ Right x

-- 5. Implement beside

beside'
  :: Traversal s t a b -> Traversal s' t' a b -> Traversal (s, s') (t, t') a b
beside' left' right' handler (s, s') =
  (,) <$> (s & left' %%~ handler) <*> (s' & right' %%~ handler)

-- I originally wanted to rewrite this as
-- beside' :: (a -> f b) -> (s, s') -> f (t, t')
-- however, that isn't the correct signature. What it should be is:
-- beside' ::
--     Traversal s t a b
--  -> Traversal s' t' a b
--  -> (a -> f b)
--  -> (s, s')
--  -> f (t, t')
-- Essentially, only rewrite the final Traversal and leave the input Traversals
-- alone.

-- 7.6 Traversal Laws

z' :: [(String, String)]
z' = traverseOf both pure ("don't", "touch")
-- [("don't", "touch")]

a_ :: Maybe (String, String)
a_ = traverseOf both pure ("don't", "touch")
-- Just ("don't", "touch")

badTupleSnd :: Traversal (Int, a) (Int, b) a b
--badTupleSnd :: (a -> f b) -> (Int, a) -> f (Int, b)
badTupleSnd h (n, a) =
  (,) <$> pure (n + 1) <*> h a

-- Identity law
-- traverseOf myTrav pure x ~ pure x

-- Composition Law
-- x & myTrav %~ f & myTrav %~ g ~ x & myTrav %~ (g . f)

-- Exercises - Traversal Laws

-- 1. `worded` is law-breaking. Which law does it break? Provide an example.

b_ = undefined

-- 2. Write a Traversal which breaks the first law

c_ :: String -> Traversal (String, a) (String, b) a b
c_ entry h (log, a) = (,) <$> pure (log ++ entry) <*> h a

-- 3. Write a Traversal which breaks the second law

d_ :: Traversal String String String String
d_ h xs = h (filter (=='x') xs)

-- 4. For each of the traversals, are they lawful? If not, come up with
-- counter-example: taking, beside, each, lined, traversed

-- Check test/Spec.hs where we tried to use `lens-properties` to
-- check the laws

-- 7.7 Advanced Manipulation

e_ = [('a', 1), ('b', 2), ('c', 3)] ^. partsOf (traversed . _1)

f_ = [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) .~ "cat"

g_ = [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) .~ "leopard"

h_ = [('a', 1), ('b', 2), ('c', 3)] & partsOf (traversed . _1) .~ "?"

-- Exercises - `partsOf`

-- Fill in the blanks

i_ = [1, 2, 3, 4] ^. partsOf (traversed . filtered even)
-- [2, 4]

j_ = ["Aardvark", "Bandicoot", "Capybara"] ^. traversed . partsOf (taking 3 traversed)
-- "AarBanCap"

-- k_ :: [Int]
k_ = ([1, 2], M.fromList [('a', 3), ('b', 4)]) ^. partsOf (beside traversed traversed)
-- [1, 2, 3, 4]

l_ = [1, 2, 3, 4] & partsOf (traversed . filtered even) .~ [20, 40]
-- [1, 20, 3, 40]

m_ = ["Aardvark", "Bandicoot", "Capybara"] & partsOf (traversed . traversed) .~ "Kangaroo"
-- ["Kangaroo","Bandicoot","Capybara"]

n_ = ["Aardvark", "Bandicoot", "Capybara"] & partsOf (traversed . traversed) .~ "Ant"
-- ["Antdvark", "Bandicoot", "Capybara"]

o_ = M.fromList [('a', 'a'), ('b', 'b'), ('c', 'c')] & partsOf traversed
  %~ \xs -> tail . take (length xs + 1) $ cycle xs

p_ = ('a', 'b', 'c') & partsOf each %~ reverse

q_ = [1, 2, 3, 4, 5, 6] & partsOf (taking 3 traversed) %~ reverse
-- [3,2,1,4,5,6]

r_ = ('a', 'b', 'c') & unsafePartsOf each %~ \xs -> fmap (xs,) xs
-- (("abc",'a'),("abc",'b'),("abc",'c'))
