{-# LANGUAGE BangPatterns #-}

collatz :: Int -> Int
collatz = go 0
  where
    go !iters !n
      | n == 1    = iters
      | even n    = go (iters+1) (n `quot` 2)
      | otherwise = go (iters+1) ((3*n) + 1)

main :: IO ()
main = print (collatz 27)

