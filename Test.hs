-- collatzRec :: (Int, Int) -> (Int, Int)
-- collatzRec (iters, n)
--   | n == 1    = (iters, n)
--   | even n    = collatzRec (iters+1, n `quot` 2)
--   | otherwise = collatzRec (iters+1, (3*n) + 1)

collatzInterp :: (Int, Int) -> Int
collatzInterp (iters, n)
  | n == 1 = iters
  | otherwise = undefined

-- collatzGo' :: (Int, Int) -> Int
-- collatzGo' = collatzInterp . collatzRec




