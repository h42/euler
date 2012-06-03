
primes :: [Integer]
primes = 2 : 3 : [x | x<- [5,7..], isPrime primes x ]

isPrime (p:ps) x
    | p^2 > x   = True
    | otherwise = if rem x p == 0 then False else isPrime ps x

-- Triangle Numbers - 1 3 6 10 15 - nth element = sum [1..n]
tris = map (\x-> quot ((x+1)*x) 2) [1..]

-- Prime factors
pfactors x' = pf x' primes [] where
    pf x (p:ps) fs
	| p > x    = fs
	| r == 0   = pf q (p:ps) (p:fs)
	| p^2 > x'  = fs
	| otherwise = pf x ps fs
      where (q,r) = quotRem x p

-- Total factor count
tfactors [] = 0
tfactors (f:fs) = tfactors' fs f 2 1

tfactors' [] oldf dupcnt tot =  dupcnt * tot
tfactors' (f:fs) oldf dupcnt tot
    | f == oldf = tfactors' fs oldf (dupcnt+1) tot
    | otherwise = tfactors' fs f 2 (dupcnt*tot)

main = do
    print $ sum $ take 1000 primes
    putStrLn "hey"

