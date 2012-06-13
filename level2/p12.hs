import Data.List

primes :: [Integer]
primes = 2 : 3 : [x | x<- [5,7..], isPrime primes x ]

isPrime (p:ps) x
    | p^2 > x   = True
    | otherwise = if rem x p == 0 then False else isPrime ps x

tris = map (\x-> quot ((x+1)*x) 2) [1..]

-- Prime factors
factors x' = pf x' primes [] where
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

proc x = do
    let f = factors x
	t = tfactors f
    if t >= 500 then
	putStrLn $ "x = " ++ show x ++ " - factors = " ++ show f
	    ++ " ==> " ++ show t
    else return ()

main = do
    --print $ nub $ subsequences $ factors 90
    mapM proc (take 20000 tris)

