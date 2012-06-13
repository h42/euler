import Primes

qsl = [2*x*x | x<-[0..]]

p46 n = p46a n qsl primes
p46a n qqs@(q:qs) pps@(p:ps)
    | q>n        = n
    | n == q + p = 0
    | p < n      = p46a n qqs ps
    | otherwise  = p46a n qs primes

main = print $ head $ filter (>0) $ map p46 [3,5..]

