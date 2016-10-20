{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module WW where

import           Data.Array.Accelerate

abs :: (Lift Exp a, a ~ Plain a) => a -> Exp a
abs = lift
{-# NOINLINE abs #-}

-- | All calls to 'rep' should be gone by the time compilation finishes.
rep :: Exp a -> a
rep _ = error "Internal error: rep called"
{-# NOINLINE rep #-}

