{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

module Freer where

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

(|>) :: Functor f => (a -> b) -> (x -> f a) -> (x -> f b)
f |> g = (f <$>) . g

(>*>) :: Applicative f => (a -> f (x -> b)) -> f x -> (a -> f b)
f >*> g = (<*> g) . f

data Freer f a where
  Pure :: a -> Freer f a
  Impure :: f x -> (x -> Freer f a) -> Freer f a

instance Functor (Freer f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Impure u x) = Impure u $ f |> x

instance Applicative (Freer f) where
  pure = Pure
  (Pure f) <*> (Pure x) = Pure $ f x
  (Pure f) <*> (Impure k x) = Impure k $ f |> x
  (Impure f x) <*> m = Impure f (x >*> m)

instance Monad (Freer f) where
  return = pure
  (Pure a) >>= k = k a
  (Impure f c) >>= k = Impure f $ c >>> k
