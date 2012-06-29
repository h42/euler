import Primes
main = print $ takeWhile (<=1000000) $ scanl (*) 1 primes
