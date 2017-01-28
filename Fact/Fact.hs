{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall -Wno-orphans #-}

import           Prelude hiding (abs)

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import qualified Data.Array.Accelerate.Interpreter as AI

import           Data.Function

import           Iter

fact :: Int -> Int
fact x = go (1, x)
  where
    go :: (Int, Int) -> Int
    go (!n, 0) = n
    go (!n, !m) = go (n*m, m-1)

main :: IO ()
main = print ((transform fact) 5)

-- | Mark where the transformation begins
transform :: a -> a
transform = id
{-# NOINLINE transform #-}

loopMarker :: a -> a
loopMarker = id
{-# NOINLINE loopMarker #-}

-- Accelerate Transformation --
{-# RULES "fix->iterLoop" [~]
    forall (f :: (a -> r) -> a -> r).
    fix f
      =
    iterLoop (done . fix f) . loopMarker . (iterLoop (doneToId (done . fix f)))
 #-}

{-# RULES "to-rec-step" [~]
    forall f x.
    done (fix f x)
      =
    step x
  #-}

{-# RULES "pair-rep-abs" [~]
    forall (x :: Int) (y :: Int).
    (,) x (rep (abs y))
      =
    rep (abs (x, y))
  #-}

{-# RULES "while-intro" [~]
    forall (f :: (Int, Int) -> Iter (Int, Int) Int)
           (g :: (Int, Int) -> Iter (Int, Int) (Int, Int))
           (x :: Exp (Int, Int)).
    abs (iterLoop f (loopMarker (iterLoop g (rep x))))
      =
    abs (iterLoop f (rep
       (A.while (abs . getCondition . f . rep)
                (abs . loopBody (doneToId f) . rep)
                x)))
  #-}

-- | Start conversion to Accelerate
{-# RULES "unit-intro" [~]
    forall (f :: (Int -> Int)) x.
    transform f x
      =
    A.indexArray (AI.run (A.unit ((abs . f . rep) (abs x)))) A.Z
  #-}

rep :: Exp a -> a
rep _ = error "rep"
{-# NOINLINE rep #-}

abs :: (a ~ A.Plain a, A.Lift Exp a) => a -> Exp a
abs = A.lift
{-# NOINLINE abs #-}


-- Conditional tracing and loop body/conditional splitting

splitLoop :: (a -> Iter a b) -> (a -> b, (a -> a, a -> Bool))
splitLoop f = (loop, (loopBody (doneToId f), getCondition . f))
  where
    loop = iterLoop f

getCondition :: Iter b a -> Bool
getCondition f = getIter f (const True) (const False)

-- | Turn the last step into an `id`
doneToId :: (a -> Iter a b) -> (a -> Iter a a)
doneToId f x = getIter (f x) step (const (done x))

loopBody :: (a -> Iter a a) -> (a -> a)
loopBody f x = getIter (f x) id id

