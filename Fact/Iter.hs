{-# LANGUAGE DeriveFunctor #-}

module Iter where

import           Control.Monad

data Iter final inter
  = Done final
  | Step inter
  deriving Functor

instance Applicative (Iter final) where
  pure  = return
  (<*>) = ap

instance Monad (Iter inter) where
  return       = Step
  Done x >>= _ = Done x
  Step i >>= f = f i

  -- NOTE: This is to silence a Core lint warning:
  Done x >>  _ = Done x
  Step _ >>  y = y

iterLoop :: (a -> Iter b a) -> a -> b
iterLoop f start =
  let Done result = go start
  in result
  where
    go x = f x >>= go

