{- Author: Richard Eisenberg
   File: All.hs

   Import this file to get access to the instance for function equality.
-}

module All where

import Test.QuickCheck   ( Arbitrary, Result(..), stdArgs, Args(..)
                         , quickCheckWithResult )
import System.IO.Unsafe  ( unsafePerformIO )

instance (Arbitrary a, Show a, Eq b) => Eq (a -> b) where
  f1 == f2 = unsafePerformIO $ do
    result <- quickCheckWithResult args (\x -> f1 x == f2 x)
    case result of
      Success {} -> return True
      Failure { output = msg } -> do
        putStrLn msg
        return False
      _ -> do
        putStrLn "Error performing function comparison"
        return False
    where
      args = stdArgs { chatty = False }
