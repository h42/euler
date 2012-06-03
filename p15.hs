{-# LANGUAGE BangPatterns  #-}


-- 40 choose 20 (40! / 20! * 20!) is BEST
main = print $ product [21..40] `quot` $ product [2..20]

-- Brute Force - VERY Slow !!! but FUN
data Dir = D Int Int | R Int Int deriving (Show)

n = 20

gfunc [] = 0

gfunc (D !x !y : ds)
    | x == n    = 1 + gfunc ds
    | otherwise = gfunc [R x y' | y' <- [y+1..n]] + gfunc ds

gfunc (R !x !y : ds)
    | y == n    = 1 + gfunc ds
    | otherwise = gfunc [D x' y | x' <- [x+1..n]] + gfunc ds

main2 = print $ gfunc [(R 0 0),(D 0 0)]
