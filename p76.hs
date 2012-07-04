n = 100
ps = [n-1,n-2..2]

f [] _ _ cnt = cnt+1
f (p:ps) i tot cnt
    | tot' < n = f (p:ps) (i+1) tot cnt'
    | tot' == n = (cnt+1)
    | otherwise = cnt
  where tot' = p*i+tot
	cnt' = f ps 0 tot' cnt

main = do
    print $ f ps 0 0 0

