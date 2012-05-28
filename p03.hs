
n  = 600851475143 :: Int
testn  = 13195 :: Int
n2 = floor $ sqrt $ fromIntegral n

primes :: [Int]
primes = 2 : 3 : [x | x<- [5,7..], isPrime primes x ]
isPrime (p:ps) x
    | p^2 > x   = True
    | otherwise = if rem x p == 0 then False else isPrime ps x


main = do
    let pr = head $ filter ffunc $ reverse $ takeWhile (<n2) primes
	ffunc x = rem n x == 0
    --print $ rem n pr
    --print $ quot n pr
    print $ pr

