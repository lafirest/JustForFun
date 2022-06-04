{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Algebra where

import Data.Kind (Constraint)
import Data.Functor.Identity
import Lift
import Handler
import qualified Control.Monad.Trans.Identity as Identity

data (f :+: g) (m :: * -> *) a = Inl (f m a) | Inr (g m a)
  deriving (Eq, Functor, Ord, Show)

infixr 4 :+:

class (sub :: (* -> *) -> (* -> *)) :<: sup where
  inj :: sub m a -> sup m a

instance f :<: f where
  inj = id

instance  {-# OVERLAPPABLE #-} (f :<: h) => f :<: (g :+: h) where
  inj = Inr . inj

instance  {-# OVERLAPPABLE #-} f :<: (f :+: g) where
  inj = Inl

send :: (eff :<: sig, Algebra sig m) => eff m a -> m a
send sig = runIdentity <$>
               alg (fmap Identity . runIdentity)
                   (inj sig)
                   (Identity ())

class Monad m => Algebra sig m | m -> sig where
  alg :: Functor ctx =>
    Handler ctx n m
    -> sig n a
    -> ctx ()
    -> m (ctx a)

instance Algebra (Lift Identity) Identity where
  alg hdl (LiftWith with) = with hdl

run :: Identity a -> a
run = runIdentity
