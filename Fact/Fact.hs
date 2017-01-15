{-# LANGUAGE Strict #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

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
main = print ((transform fact) 5)

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

  -- NOTE: This is very inefficient because it calculates each step twice
eitherWhile :: forall a r. (A.Elt a, A.Lift Exp r, A.Unlift Exp a, a ~ A.Plain a, r ~ A.Plain r)
               => (a -> Either r a) -> (Exp a -> Exp r)
eitherWhile f = A.lift1 calcFinal . A.while (A.lift1 condTest) (A.lift1 body)
  where
    condTest :: a -> Bool
    condTest x =
      case f x of
        Left _ -> False
        _      -> True

    body :: a -> a
    body x =
      case f x of
        Right x' -> x'
        _        -> error "eitherWhile.body"

    calcFinal :: a -> r
    calcFinal x =
      case f x of
        Left x' -> x'
        _       -> error "eitherWhile.calcFinal"

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

