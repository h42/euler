{-# LANGUAGE BangPatterns#-}

module Primes (
    primes
    ,isPrime
    ,rotateInt
    ,primeSet
    ,gotPrime
) where

import qualified Data.IntSet as S

-----------------------------
-- Factors
-----------------------------
dfactors zn = dfactors' primes [] zn where  -- Distinct Prime Factors
    dfactors' pps@(p:ps) as n
	| n==1 = as
	| p*p > zn = if n /= zn then n:as else as
	| otherwise = dfactors' ps' as' n'
       where
	    (q,r) = quotRem n p
	    a = if null as then 1 else head as
	    as' = if r==0 && a /= p then p:as else as
	    n' = if r==0 then q else n
	    ps' = if r==0 then pps else ps

totient n = if not (null zfs) then tot zfs n else n-1 where
    zfs = dfactors n
    tot [] ans = ans
    tot (f:fs) ans = tot fs (quot (ans * (f-1)) f)

farey n = sum $ map totient [2..n] -- farey sequence = |F(n)| = |F(n-1)| + totient n

-----------------------------
-- SET OPS
-----------------------------
primeSet n = S.fromList $ takeWhile (<=n) primes

gotPrime x set = S.member x set

-----------------------------
-- PRIMES
-----------------------------
-- primes :: [Int]
primes = 2 : 3 : (primes2 6)
primes2 gen  = ans where
    !a = gen - 1
    !b = gen + 1
    !pa = isPrime2 primes a
    !pb = isPrime2 primes b
    !ans
	| pa && pb  = a : b : primes2 (gen+6)
	| pa        = a : primes2 (gen+6)
	| pb        = b : primes2 (gen+6)
	| otherwise = primes2 (gen+6)

isPrime x
    | x > 2  = isPrime2 primes x
    | x == 2 = True
    | otherwise = False

isPrime2 (p:ps) x   -- 1.575
    | rem x p == 0 = False
    | p*p > x      = True
    | otherwise    = isPrime2 ps x

rotateInt x e = y where
    (q,r) = quotRem x 10
    y = q + r * 10^e

