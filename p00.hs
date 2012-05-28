
primes :: [Integer]
primes = 2 : 3 : [x | x<- [5,7..], isPrime primes x ]

isPrime (p:ps) x
    | p^2 > x   = True
    | otherwise = if rem x p == 0 then False else isPrime ps x

main = do
    print $ sum $ take 1000 primes
    putStrLn "hey"

