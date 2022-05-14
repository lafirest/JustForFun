{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module EIO where

import Eff
import Data.Sum

data EIO a where
  Print :: String -> EIO ()

print :: EIO :< r => String -> Eff r ()
print = send . Print

runIO :: Eff '[EIO] w -> IO w
runIO (Pure x) = return x
runIO (Eff r q) = case decomposeLast r of
                    Print str -> putStrLn str >> runIO (qApp q ())
