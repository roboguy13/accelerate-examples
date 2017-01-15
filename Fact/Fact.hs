{-# LANGUAGE Strict #-}

import           Data.Array.Accelerate (Exp)
import qualified Data.Array.Accelerate as A

import           Data.Function

fact :: Int -> Int
fact x = go (1, x)
  where
    go :: (Int, Int) -> Int
    go (n, 0) = n
    go (n, m) = go (n*m, m-1)

main :: IO ()
main = print (transform (fact 5))

-- | Mark where the transformation begins
transform :: a -> a
transform = id
{-# NOINLINE transform #-}

-- Accelerate Transformation --

eitherRec :: (a -> Either r a) -> (a -> r)
eitherRec step initial
  = let Left result = go initial
    in result
  where
    go x = step x >>= go

{-# RULES "fix->eitherRec" [~]
    forall (f :: (a -> r) -> a -> r).
    fix f
      =
    eitherRec (Left . fix f)
 #-}

{-# RULES "to-rec-step" [~]
    forall f x.
    Left (fix f x)
      =
    Right x
  #-}

