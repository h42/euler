import qualified Data.IntSet as S

sieve = 2 : filter (==3) [3,5..]

primes :: [Int]
primes = 2 : 3 : (primes2 6)
primes2 gen  = ans where
    a = gen - 1
    b = gen + 1
    pa = isPrime2 primes a
    pb = isPrime2 primes b
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

primeSet n = S.fromList $ takeWhile (<=n) primes
gotPrime x set = S.member x set

main = do
    print $ last (takeWhile (<5000000) primes)
