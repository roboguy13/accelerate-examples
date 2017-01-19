{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import           Data.Function

import           Iter

fact :: Int -> Int
fact x = go (1, x)
  where
    go :: (Int, Int) -> Int
    go (n, 0) = n
    go (n, m) = go (n*m, m-1)

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
    iterLoop (Done . fix f)
 #-}

{-# RULES "to-rec-step" [~]
    forall f x.
    Done (fix f x)
      =
    Step x
  #-}

