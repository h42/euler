factors :: Int -> [Int]
factors n = fact' 2 [1] where
    fact' i fs = ans where
	(q,r) = quotRem n i
	ans = if i^2 > n then fs
	      else if r /= 0 then fact' (i+1) fs
	      else if i == q then i:fs
	      else fact' (i+1) (i:q:fs)

factorSum n
    | n < 2 = 0
    | otherwise =  sum $ factors n

amicable n = if n == fs2 && n /= fs1  then n else 0 where
    fs1 = factorSum n
    fs2 = factorSum fs1

main = print $ sum $ filter (>2) (map amicable [3..10000])

