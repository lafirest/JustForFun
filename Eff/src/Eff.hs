{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Eff where

import FTCQueue
import Data.Sum
import FTCQueue (tsingleton)

type Arr r a b = a -> Eff r b
type Arrs r a b = FTCQueue (Eff r) a b

data Eff r a where
  Pure :: a -> Eff r a
  Eff :: Sum r x -> Arrs r x b -> Eff r b


instance Functor (Eff r) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Eff r q) = Eff r (q |> (Pure . f))

instance Applicative (Eff r) where
  pure = Pure
  Pure f <*> Pure x = Pure $ f x
  Pure f <*> Eff r q = Eff r $ (q |> (Pure . f))
  Eff r q <*> Pure x = Eff r (q |> (Pure . ($ x)))
  Eff r q <*> m = Eff r (q |> (<$> m))

instance Monad (Eff r) where
  return = pure
  (Pure a) >>= k = k a
  (Eff r q) >>= k = Eff r (q |> k)

send :: f :< r => f v -> Eff r v
send f = Eff (inject f) (tsingleton Pure)

qApp :: Arrs r x b -> x -> Eff r b
qApp q x =
  case tviewl q of
    TOne k  -> k x
    k :| t -> case k x of
      Pure y -> qApp t y
      Eff r q' -> Eff r (q' >< t)

qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
qComp q h = h . qApp q

handleRquest :: (a -> Eff r w) ->
  (forall v. f v -> Arr r v w -> Eff r w) ->
  Eff (f ': r) a -> Eff r w
handleRquest ret h = loop
  where
    loop (Pure x) = ret x
    loop (Eff r q) = case decompose r of
      Right f -> h f k
      Left r' -> Eff r' (tsingleton k)
      where k = qComp q loop

runEff (Pure x) = x
