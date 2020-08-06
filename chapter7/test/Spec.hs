{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Test.QuickCheck
import           Control.Lens
import           Control.Lens.Properties
import           Lib

prop_worded = isTraversal (worded :: Traversal' String String)
prop_c_ = isTraversal (c_ "hello" :: Traversal' (String, Int) Int)
prop_d_ = isTraversal (d_ :: Traversal' String String)

-- Unsure how to do `taking` and `beside`
prop_each = isTraversal (each :: Traversal' (Int, Int) Int)
prop_lined = isTraversal (lined :: Traversal' String String)
prop_traversed = isTraversal (traversed :: Traversal' (Int, Int) Int)

main :: IO ()
main = do
  putStrLn "should fail"
  quickCheck prop_worded

  putStrLn "should fail"
  quickCheck prop_c_

  putStrLn "should fail"
  quickCheck prop_d_

  putStrLn "should pass"
  quickCheck prop_each

  putStrLn "should fail"
  quickCheck prop_lined

  putStrLn "should pass"
  quickCheck prop_traversed
