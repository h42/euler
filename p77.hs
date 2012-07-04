import Primes

f n = f2 ps' 0 0 0 where
    ps' = tail $ takeWhile (<=n) primes -- drop 2

    f2 [] _ tot cnt = if even tot then cnt+1 else cnt
    f2 (p:ps) i tot cnt
	| tot' < n = f2 (p:ps) (i+1) tot cnt'
	| tot' == n = (cnt+1)
	| otherwise = cnt
      where tot' = p*i+tot
	    cnt' = f2 ps 0 tot' cnt

main = print $ head $ filter (\n->f n >= 5000) [50..]


