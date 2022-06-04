{-# LANGUAGE RankNTypes #-}

module Handler where

type Handler ctx m n = forall x . ctx (m x) -> n (ctx x)
