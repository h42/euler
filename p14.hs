{-# LANGUAGE BangPatterns  #-}

collatz :: Int -> Int ->    Int
collatz !cnt !1 = cnt
collatz !cnt !n
    | even n    = collatz (cnt+1) (quot n 2)
    | otherwise = collatz (cnt+1) (3 * n + 1)

--main = print $ maximum $ map (\x->(collatz 1 x,x)) [1..999999]

col2 :: Int -> Int -> Int -> (Int,Int)
col2 !n !i !m
    | n == 500000 = (i,m)
    | otherwise = col2 (n-1) i' m'
  where
    !x = collatz 1 n
    !(!i',!m') = if x > m then (n,x) else (i,m)

main = print $ col2 999999 0 0

