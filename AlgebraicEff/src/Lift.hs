{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Lift where

import Handler

data Lift sig m k where
  LiftWith :: (forall ctx . Functor ctx => Handler ctx m sig -> ctx () -> sig (ctx a)) -> Lift sig m a
