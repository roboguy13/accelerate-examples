{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

import           Prelude hiding (abs)

import           Data.Array.Accelerate hiding (even)
import qualified Data.Array.Accelerate as A
import           Data.Function
import           GHC.Exts (inline)

import           WW

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

data Annotation :: * -> * where
  Start       :: a -> Annotation a
  Recursive   :: Annotation () -- | Mark something as recursive
  RecFun      :: Annotation ()
  RecCall     :: Annotation () -- | Mark a recursive call
  RecCondF    :: (Exp a -> Exp Bool) -> (Exp Bool) -> Annotation a
  RecCondT    :: (Exp a -> Exp Bool) -> (Exp Bool) -> Annotation a
  RecCondBoth :: (Exp a -> Exp Bool) -> (Exp Bool) -> Annotation a

  BaseCase    :: Annotation () -- | Used in type transformation of recursive body

annotate :: Annotation a -> b -> b
annotate _ b = b
{-# NOINLINE annotate #-}

-- | This is intended to be partially evaluated by the HERMIT script.
combineAnns :: Annotation a -> Annotation a -> b -> b
combineAnns (RecCondF cf1 c1) (RecCondT cf2 c2) b =
  annotate (RecCondBoth (\arg -> cf1 arg ||* cf2 arg) (c1 ||* c2))
           b

combineAnns (RecCondT cf1 c1) (RecCondF cf2 c2) b =
  annotate (RecCondBoth (\arg -> cf1 arg ||* cf2 arg) (c1 ||* c2))
           b

-- combineAnns (RecCondF cf1 c1) (RecCondF cf2 c2) b =
combineAnns BaseCase a@(RecCondF _ _) b = annotate a b
combineAnns BaseCase a@(RecCondT _ _) b = annotate a b
combineAnns BaseCase a@(RecCondBoth _ _) b = annotate a b
combineAnns BaseCase RecCall b = annotate RecCall b
combineAnns BaseCase _ b = annotate BaseCase b
combineAnns _ _ _ = error "Internal error: Invalid annotation combination"
{-# NOINLINE combineAnns #-}

dummyArg :: a
dummyArg = error "Internal error: dummyArg"
{-# NOINLINE dummyArg #-}

dummyRecFun :: a
dummyRecFun = error "Internal error: dummRecFun"

whileCond :: a
whileCond = error "Internal error: whileCond"
{-# NOINLINE whileCond #-}


-- Transformation RULES:

-- Initialization
{-# RULES "Acc-Start" [~]
    forall f (init :: Int).
    transform (f init)
      =
    rep (annotate (Start init)
                  (abs (inline f dummyArg)) :: Exp Int)
  #-}

-- General elimination
{-# RULES "abs-rep-elim" [~]
    forall x.
    abs (rep x) = x
  #-}

{-# RULES "inline-elim" [~]
    forall x.
    inline x = x
  #-}

-- -- General transformation
{-# RULES "arg-float-in" [~]
    forall (a :: Annotation ()) (f :: (Int, Int) -> Exp Int) x.
    annotate a f x
      =
    annotate a (f x)
  #-}

-- Basic operations
{-# RULES "==*-intro" [~]
    forall (x :: Int) y.
    x == y
      =
    rep (abs x A.==* abs y)
  #-}

{-# RULES "even-intro" [~]
    forall (x :: Int).
    even x
      =
    rep (A.even (abs x))
  #-}

{-# RULES "+-intro" [~]
    forall (x :: Int) y.
    x + y
      =
    rep (abs x + abs y)
  #-}

{-# RULES "*-intro" [~]
    forall (x :: Int) y.
    x * y
      =
    rep (abs x * abs y)
  #-}

{-# RULES "quot-intro" [~]
    forall (x :: Int) y.
    quot x y
      =
    rep (quot (abs x) (abs y))
  #-}

-- Conditionals
{-# RULES "abs-if->cond" [~]
    forall b (t :: Int) f.
    abs (case b of True -> t; False -> f)
      =
    cond (abs b) (abs t) (abs f)
  #-}

{-# RULES "cond-RecCondBoth" [~]
    forall (c :: Exp (Int, Int) -> Exp Bool) x t f.
    cond (c x) (annotate RecCall t) (annotate RecCall f)
      =
    annotate (RecCondBoth c (c x))
             (cond (c x)
                   t
                   f)
  #-}

{-# RULES "cond-RecCondF" [~]
    forall c (x :: Exp Int) t f.
    cond (c x) t (annotate RecCall f)
      =
    annotate (RecCondF (A.not . c) (c x))
             (cond (c x)
                   t
                   f)
  #-}


{-# RULES "RecCondF-float-else" [~]
    forall c x accCond c' t t' f'.
    cond (c x)
         t
         (annotate (RecCondF accCond c')
                   (cond c' t' f'))
      =
    annotate (RecCondF (\arg -> A.not (c arg) &&* accCond arg) (c x))
             (cond (c x)
                   t
                   (cond c' t' f'))
  #-}


{-# RULES "RecCondF-elim" [~]
    forall accCond c t f.
    annotate (RecCondF accCond c)
             (cond c t f)
      =
    cond c t f
  #-}

{-# RULES "combineAnns-intro" [~]
    forall a1 a2 b.
    annotate a1 (annotate a2 b)
      =
    combineAnns a1 a2 b
  #-}

-- Recursion
{-# RULES "fix-abs-rep-intro" [~]
    forall (f :: ((Int, Int) -> Int) -> (Int, Int) -> Int) (a :: (Int, Int)).
    abs (fix f a)
      =
    fix (\fRec -> annotate RecFun (abs . (f (rep . fRec)))) a
  #-}

{-# RULES "RecCall-intro" [~]
    forall (f :: ((Int, Int) -> Exp Int) -> (Int, Int) -> Exp Int) (arg :: (Int, Int)).
    fix f arg
      =
    annotate Recursive (f (annotate RecCall . f (fix f))) arg
  #-}

{-# RULES "while-intro" [~]
    forall f (arg :: (Int, Int)).
    annotate Recursive (annotate RecFun f arg)
      =
    while whileCond
          (\__REC_ARG__ -> f (rep __REC_ARG__))
          (abs arg)
  #-}

-- Pairs

efirst, esecond :: (a, a) -> a
efirst  (x, _) = x
esecond (_, y) = y

{-# RULES "pair-rep" [~]
    forall (x :: Exp (Float, Float)).
    rep x
      =
    (efirst (rep x), esecond (rep x))
  #-}

