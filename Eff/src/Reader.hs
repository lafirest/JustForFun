{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Reader where
import Eff
import Data.Sum

data Reader r a where
  Get :: Reader r r

runReader :: forall x r w.Eff (Reader x ': r) w -> x -> Eff r w
runReader e r = handleRquest return handle e
  where
    handle :: Reader x v -> Arr r v w -> Eff r w
    handle Get k = k r

ask :: (Reader i :< r) => Eff r i
ask = send Get
