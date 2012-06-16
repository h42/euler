{-# LANGUAGE BangPatterns#-}
module Primes (
    primes
    ,isPrime
    ,num2list
    ,list2num
    ,rotateList
    ,rotateInt
) where

primes :: [Int]
--aprimes = 2 : 3 : [x | x<- [5,7..], isPrime2 primes x ]
primes = 2 : 3 : (primes' 6)
primes' gen  = ans where
    a = gen - 1
    b = gen + 1
    pa = isPrime primes a
    pb = isPrime primes b
    !ans
	| pa && pb  = a : b : primes' (gen+6)
	| pa        = a : primes' (gen+6)
	| pb        = b : primes' (gen+6)
	| otherwise = primes' (gen+6)

-- does not include <2 check that exported version has
isPrime2 (ps) 2 = True
isPrime2 (p:ps) x
    | rem x p == 0 = False
    | p*p > x      = True
    | otherwise    = isPrime ps x

isPrime (ps) 2 = True
isPrime (p:ps) x
    | rem x p == 0 || x<2 = False
    | p*p > x      = True
    | otherwise    = isPrime ps x

num2list n = num2list' n []
num2list' 0 xs = xs
num2list' n xs = num2list' q (r:xs) where (q,r) = quotRem n 10

list2num xs = foldl (\a x->a*10+x) 0 xs  -- works for positive numbers

rotateList (x:xs) = xs++[x]

rotateInt x e = y where
    (q,r) = quotRem x 10
    y = q + r * 10^e

