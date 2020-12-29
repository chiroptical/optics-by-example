module Lib where

import Control.Lens

a = [(1, True), (2, False), (3, True)] & traversed . _1 *~ 10

b = view _1 ('a', 2)

c = view _1 ("abc", 123)

d = ("old", False) & _1 .~ "new"

e = view (_1 . _3) (('a', 'b', 'c'), 'd')

f = ("blue", Just (2 :: Int)) ^? _2 . _Just

g = ['a' .. 'z'] ^.. taking 5 folded

h = traverseOf_ both putStrLn ("one", "two")
