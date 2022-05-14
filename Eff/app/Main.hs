{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (print)
import Eff
import Reader
import EIO
import Data.Sum
import Text.Printf
import GHC.Float (float2Int)

testR1 :: (Reader Int :< r, EIO :< r) => Eff r Int
testR1 = do
  r <- ask
  print $ printf "in testR1, r is:%d" r
  return (r + 1)

testR2 :: (Reader Float :< r, EIO :< r) => Eff r Float
testR2 = do
  r <- ask
  print $ printf "in testR2, r is:%f" r
  return (r * 2)

testR3 :: (Reader Float :< r, Reader Int :< r, EIO :< r) => Eff r Int
testR3 = do
  x <- testR1
  y <- testR2
  print $ "This function has 3 monads"
  return (x + float2Int y)

main :: IO ()
main = do
  x <- runIO $ runReader (runReader testR3 (2 :: Int)) (3 :: Float)
  putStrLn $ printf "value is:%d" x
  y <- runIO $ runReader (runReader testR3 (3 :: Float)) (2 :: Int)
  putStrLn $ printf "value is:%d" y
  return ()
