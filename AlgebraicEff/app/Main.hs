{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Algebra
import Reader
import Text.Printf

test :: ((Reader Int) :<: sig, Algebra sig m) => m Int
test = do
  x <- ask
  return x

test2 :: Int
test2 = run . runReader (3 :: Int) $ test

main :: IO ()
main = do
  printf "test2 result is:%d\n" test2
  return ()
