import Primes

combinations k n = combinationsOf k [0..n-1] where
    combinationsOf 0 _ = [[]]
    combinationsOf _ [] = []
    combinationsOf k' (x:xs) = map (x:)
	(combinationsOf (k'-1) xs) ++ combinationsOf k' xs

main = do
    --print $ last (takeWhile (<5000000) primes)
    print $ maximum $ map  (length.pfactors) [2,4..150000]

