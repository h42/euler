import Primes

main = do
    print $ sum $ take 21 primes
    print $ length $ takeWhile (<1000000) primes


