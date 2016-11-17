{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

import           Prelude hiding (abs)

import           Data.Array.Accelerate hiding (even)
import qualified Data.Array.Accelerate as A
import           Data.Function
import           GHC.Exts (inline)

import           WW
import           Data.Proxy

collatz :: Int -> Int
collatz arg = go (0, arg)
  where
    go :: (Int, Int) -> Int
    go (iters, n)
      | n == 1    = iters
      | even n    = go ((iters+1), (n `quot` 2))
      | otherwise = go ((iters+1), ((3*n) + 1))

main :: IO ()
main = print (transform (collatz 27))

-- | Mark where the transformation begins
transform :: a -> a
transform = id
{-# NOINLINE transform #-}


-- Accelerate transformation --

recCall :: a -> a
recCall = error "Internal error: 'recCall' called"
{-# NOINLINE recCall #-}

condAnn :: Elt a => (b -> Exp Bool) -> Exp Bool -> Exp a -> Exp a -> Exp a
condAnn = error "Internal error: 'condAnn' called"
{-# NOINLINE condAnn #-}

condTyAnn :: Elt a => Proxy b -> Exp Bool -> Exp a -> Exp a -> Exp a
condTyAnn = error "Internal error: 'condTyAnn' called"
{-# NOINLINE condTyAnn #-}

eqAnn :: a ~ b => Proxy (a ~ b) -> a -> a
eqAnn Proxy x = x
{-# NOINLINE eqAnn #-}

safeCoerce :: Proxy (a ~ b) -> ((a ~ b) => a -> b)
safeCoerce Proxy x = x
{-# NOINLINE safeCoerce #-}

condBool :: a -> a
condBool = error "Internal error: 'condBool' called"
{-# NOINLINE condBool #-}

cond' :: Elt a => Exp Bool -> Exp a -> Exp a -> Exp a
cond' = error "Internal error: cond' called"
{-# NOINLINE cond' #-}


-- Transformation RULES --

-- General purpose rules:
{-# RULES "abs-rep-elim" [~]
    forall x.
    abs (rep x) = x
  #-}

{-# RULES "abs-recCall-float" [~]
    forall x.
    abs (recCall x) = recCall (abs x)
  #-}

{-# RULES "rep-abs-pair" [~]
    forall (p :: (Int, Int)).
    rep (abs p)
      =
    p
  #-}

{-# RULES "==*-intro" [~]
    forall a (b :: Int).
    a == b
      =
    rep (abs a ==* abs b)
  #-}

{-# RULES "+-intro" [~]
    forall a (b :: Int).
    a + b
      =
    rep (abs a + abs b)
  #-}

{-# RULES "*-intro" [~]
    forall a (b :: Int).
    a * b
      =
    rep (abs a * abs b)
  #-}


{-# RULES "even-intro" [~]
    forall (a :: Int).
    Prelude.even a
      =
    rep (A.even (abs a))
  #-}

-- A trick to target the conditional of a 'cond':
{-# RULES "condBool-intro" [~]
    forall b t f.
    cond b t f
      =
    cond' (condBool b) t f
  #-}

{-# RULES "condBool-elim" [~]
    forall b.
    condBool b = b
  #-}

{-# RULES "cond'-elim" [~]
    forall b t f.
    cond' b t f = cond b t f
  #-}

{-# RULES "abs-recCall-commute" [~]
    forall (x :: Int).
    abs (recCall x)
      =
    recCall (abs x)
  #-}

-- Specific transformation steps:

{-# RULES "Acc-start" [~]
    forall (x :: Int).
    transform x
      =
    rep (abs x)
  #-}


{-# RULES "abs-fix" [~]
    forall (f :: ((Int, Int) -> Int) -> (Int, Int) -> Int) (arg :: (Int, Int)).
    abs (fix f arg)
      =
    fix (\fRec -> abs . f (rep . fRec)) arg
  #-}


{-# RULES "recCall-intro" [~]
    forall (f :: ((Int, Int) -> Int) -> (Int, Int) -> Int) (arg :: (Int, Int)).
    fix f arg
      =
    rep (fix (\fRec __REC_ARG__ -> (abs . f (recCall . rep . fRec . abs) . rep) __REC_ARG__) (abs arg))
  #-}
    -- f (\__REC_ARG__ -> (recCall . f (fix f)) __REC_ARG__) (rep (abs arg))

{-# RULES "pair-canonical" [~]
    forall (p :: Exp (Int, Int)).
    rep p
      =
    (rep (A.fst p), rep (A.snd p))
  #-}

{-# RULES "pair-lift" [~]
    forall (x :: Exp Int) (y :: Exp Int).
    abs (rep x, rep y)
      =
    lift (x, y)
  #-}

{-# RULES "abs-if->cond" [~]
    forall b t (f :: Int).
    abs (if b then t else f)
      =
    cond (abs b) (abs t) (abs f)
  #-}

{-# RULES "cond-rec-both" [~]
    forall c (x :: Exp (Int, Int)) t (f :: Exp Int).
    cond (c x) (recCall t) (recCall f)
      =
    condAnn (\ (_ :: Exp (Int, Int)) -> lift True) (c x) (recCall t) (recCall f)
  #-}

{-# RULES "cond-rec-true" [~]
    forall c (x :: Exp (Int, Int)) t f.
    cond (c x) (recCall t) f
      =
    condAnn (\z -> c z) (c x) (recCall t) f
  #-}

{-# RULES "cond-rec-false" [~]
    forall c (x :: Exp (Int, Int)) t (f :: Exp Int).
    cond (c x) t (recCall f)
      =
    condAnn (\z -> A.not (c z)) (c x) t (recCall f)
  #-}

-- Combination rule:
{-# RULES "combine-conds" [~]
    forall (cf1 :: a -> Exp Bool) c1 cf2 c2 t1 f1 cf3 c3 t2 (f2 :: Exp Int).
    condAnn cf1 c1 (condAnn cf2 c2 t1 f1) (condAnn cf3 c3 t2 f2)
      =
    condAnn (\(z :: a) -> (cf1 z &&* cf2 z) ||* (A.not (cf1 z) &&* cf3 z))
            c1
            (condTyAnn (Proxy @a) c2 t1 f1)
            (condTyAnn (Proxy @a) c3 t2 f2)
  #-}

{-# RULES "condAnn->condTyAnn" [~]
    forall (cf :: a -> Exp Bool) c t f.
    condAnn cf c t f
      =
    condTyAnn (Proxy @a) c t f
  #-}

{-# RULES "cond-condAnn-true" [~]
    forall c1 (x :: Exp (Int, Int)) c2 t f1 f2 cf.
    cond (c1 x) (condAnn cf c2 t f1) f2
      =
    condAnn (\z -> c1 z &&* cf z) (c1 x) (condAnn cf c2 t f1) f2
  #-}

{-# RULES "cond-condAnn-false" [~]
    forall c1 (x :: Exp (Int, Int)) c2 t1 f t2 cf.
    cond (c1 x) t1 (condAnn cf c2 t2 f)
      =
    condAnn (\z -> A.not (c1 z) &&* cf z) (c1 x) t1 (condAnn cf c2 t2 f)
  #-}


{-# RULES "cond-no-rec" [~]
    forall c (x :: Exp (Int, Int)) t f.
    cond (c x) t f
      =
    condAnn (\ (_ :: Exp (Int, Int)) -> lift False) (c x) t f
  #-}

-- Recursive call elimination (for base case)
{-# RULES "cond-rec-both-elim" [~]
    forall cf c t f.
    condAnn cf c (recCall t) (recCall f)
      =
    error "Internal error: Invalid state in base case"
  #-}

{-# RULES "cond-rec-true-elim" [~]
    forall cf c t f.
    condAnn cf c (recCall t) f
      =
    f
  #-}

{-# RULES "cond-rec-true-elim" [~]
    forall cf c t f.
    condAnn cf c t (recCall f)
      =
    t
  #-}

-- -- Type generalization
-- {-# RULES "eq-Proxy-intro" [~]
--     forall (p :: Proxy argTy) c (t :: b) f.
--     condTyAnn p c t f
--       =
--     eqAnn (Proxy :: Proxy (argTy ~ b))
--           (cond c t f)
--   #-}

