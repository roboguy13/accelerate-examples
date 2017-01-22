{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wall #-}

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

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

-- Accelerate Transformation --

{-# RULES "fix->iterLoop" [~]
    forall (f :: (a -> r) -> a -> r).
    fix f
      =
    fst (splitLoop (done . fix f))
 #-}
    -- iterLoop (done . fix f)

{-# RULES "to-rec-step" [~]
    forall f x.
    done (fix f x)
      =
    step x
  #-}


-- Conditional tracing and loop body/conditional splitting

splitLoop :: (a -> Iter a b) -> (a -> b, (a -> Bool))
splitLoop f = (iterLoop f, getCondition . f)

getCondition :: Iter b a -> Bool
getCondition f = getIter f (const True) (const False)

-- rep :: Exp a -> a
-- rep = error "rep"

-- {-# RULES "while-intro" [~]
--     forall (f :: (A.Elt a) => a -> b).
--     fst (splitLoop f)
--       =
--     rep . A.while (A.lift . getCondition . f . unlift) undefined
--   #-}





-- {-# RULES "to-rec-cond" [~]
--     forall f.
--     iterLoop f
--       =
--     (\c ->
--       iterLoop f)
--       (getCondition . f)
--   #-}


-- {-# RULES "Step-case-Bool" [~]
--     forall b t f.
--       Step (case b of
--               True  -> t
--               False -> f)
--       =


