module Main where

import Reader

main :: IO ()
main =
  putStrLn $ show $ run test 10
