module Primes (
    primes
    ,isPrime
) where

primes :: [Int]
--aprimes = 2 : 3 : [x | x<- [5,7..], isPrime2 primes x ]
primes = 2 : 3 : (primes' 6)
primes' gen  = ans where
    a = gen - 1
    b = gen + 1
    pa = isPrime primes a
    pb = isPrime primes b
    ans
	| pa && pb  = a : b : primes' (gen+6)
	| pa        = a : primes' (gen+6)
	| pb        = b : primes' (gen+6)
	| otherwise = primes' (gen+6)

isPrime2 (p:ps) x -- does not include <2 check that exported version has
    | rem x p == 0 = False
    | p*p > x      = True
    | otherwise    = isPrime ps x

isPrime (p:ps) x
    | rem x p == 0 || x<2 = False
    | p*p > x      = True
    | otherwise    = isPrime ps x

