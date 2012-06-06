primes :: [Int]
primes = 2 : 3 : [x | x<- [5,7..], isPrime primes x ]
isPrime (p:ps) x
    | p*p > x   = True
    | otherwise = if rem x p == 0 then False else isPrime ps x

main = do
    print $ head $ drop 100000 primes

