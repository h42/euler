--
-- FACTORS
--
factors :: Int -> [Int]
factors n = fact' 2 [1] where
    fact' i fs = ans where
	(q,r) = quotRem n i
	ans = if i^2 > n then fs
	      else if r /= 0 then fact' (i+1) fs
	      else if i == q then i:fs
	      else fact' (i+1) (i:q:fs)

--
-- PRIME FACTORS
--
pfactors x' = pf x' primes [] where
    pf x (p:ps) fs
	| p > x    = fs
	| r == 0   = pf q (p:ps) (p:fs)
	| p^2 > x'  = fs
	| otherwise = pf x ps fs
      where (q,r) = quotRem x p

--
-- PRIMES
--
primes :: [Integer]
primes = 2 : 3 : [x | x<- [5,7..], isPrime primes x ]

isPrime (p:ps) x
    | p^2 > x   = True
    | otherwise = if rem x p == 0 then False else isPrime ps x

--
-- TOTAL FACTOR COUNT
--
tfactors [] = 0
tfactors (f:fs) = tfactors' fs f 2 1

tfactors' [] oldf dupcnt tot =  dupcnt * tot
tfactors' (f:fs) oldf dupcnt tot
    | f == oldf = tfactors' fs oldf (dupcnt+1) tot
    | otherwise = tfactors' fs f 2 (dupcnt*tot)

