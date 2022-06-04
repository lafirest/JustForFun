{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Reader where
import Algebra
import Control.Applicative (liftA2)

data Reader r (m :: * -> *) k where
  Ask :: Reader r m r

ask :: ((Reader r) :<: sig, Algebra sig m) => m r
ask = send Ask

runReader :: r -> ReaderC r m a -> m a
runReader r (ReaderC runReaderC) = runReaderC r

newtype ReaderC r m a = ReaderC (r -> m a)
  deriving (Functor)

instance Applicative m => Applicative (ReaderC r m) where
  pure = ReaderC . const . pure
  liftA2 f (ReaderC a) (ReaderC b) = ReaderC $ \ r ->
    liftA2 f (a r) (b r)
  ReaderC f <*> ReaderC a = ReaderC (liftA2 (<*>) f a)

instance Monad m => Monad (ReaderC r m) where
  ReaderC a >>= f = ReaderC (\ r -> a r >>= runReader r . f)

instance Algebra sig m => Algebra (Reader r :+: sig) (ReaderC r m) where
  alg hdl sig ctx = ReaderC $ \ r -> case sig of
    Inl Ask         -> pure (r <$ ctx)
    Inr other       -> alg (runReader r . hdl) other ctx

