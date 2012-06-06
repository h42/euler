primes :: [Int]
primes = 2 : 3 : [x | x<- [5,7..], isPrime x ]

isPrime x = isPrime' primes where
    x2 = floor $ sqrt $ fromIntegral x
    isPrime' (p:ps)
	| p > x2    = True
	| otherwise = if rem x p == 0 then False else isPrime' ps
    
main = print $ head $ drop 100000 primes
