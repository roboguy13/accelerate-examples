{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

import           Prelude hiding (abs)

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A
import           Data.Function

import           WW

collatz :: Int -> Int
collatz = go 0
  where
    go !iters !n
      | n == 1    = iters
      | even n    = go (iters+1) (n `quot` 2)
      | otherwise = go (iters+1) ((3*n) + 1)

main :: IO ()
main = print (transform (collatz 27))

-- | Mark where the transformation begins
transform :: a -> a
transform = id
{-# NOINLINE transform #-}


-- Accelerate transformation --

recFun :: a -> a
recFun = id
{-# NOINLINE recFun #-}

dummyArg :: a
dummyArg = error "Internal error: dummyArg"
{-# NOINLINE dummyArg #-}

start :: a -> b -> b
start _ b = b
{-# NOINLINE start #-}

-- | Mark something as recursive
recursive :: a -> a
recursive _ = error "Internal error: 'recursive' called"
{-# NOINLINE recursive #-}

-- | Mark the recursive call
recCall :: a -> a
recCall = id
{-# NOINLINE recCall #-}

whileCond :: a
whileCond = error "Internal error: whileCond"
{-# NOINLINE whileCond #-}

-- Transformation RULES:

-- Initialization
{-# RULES "Acc-start" [~]
    forall f (init :: Int).
    transform (f init)
      =
    rep (abs (start init (f dummyArg)) :: Exp Int)
  #-}

-- Basic operations
{-# RULES "==*-intro"
    forall (x :: Int) y.
    x == y
      =
    rep (abs x A.==* abs y)
  #-}

{-# RULES "even-intro"
    forall (x :: Int).
    even x
      =
    rep (A.even (abs x))
  #-}

{-# RULES "+-intro"
    forall (x :: Int) y.
    x + y
      =
    rep (abs x + abs y)
  #-}

{-# RULES "*-intro"
    forall (x :: Int) y.
    x * y
      =
    rep (abs x * abs y)
  #-}

{-# RULES "quot-intro"
    forall (x :: Int) y.
    quot x y
      =
    rep (quot (abs x) (abs y))
  #-}


-- Recursion
{-# RULES "fix-abs-rep-intro" [~]
    forall f (a :: (Float, Float, Float)).
    abs (fix f a)
      =
    (fix (\fRec -> recFun (\x -> abs (f (rep . fRec) x)))) a
  #-}

{-# RULES "recCall-intro" [~]
    forall f (arg :: Int).
    fix f arg
      =
    recursive (f (\x -> recCall (abs x)) arg)
  #-}

{-# RULES "while-intro" [~]
    forall f (arg :: Int).
    recursive (recFun f arg)
      =
    A.while whileCond
            (\__REC_ARG__ -> f (rep __REC_ARG__))
            (abs arg)
  #-}

