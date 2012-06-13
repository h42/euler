{-
longdiv _ _ 0 ans = reverse ans
longdiv dd divisor l ans = ans' where
    (q,r) = quotRem dd divisor
    ans' = if r == 0 then reverse (q:ans)
	  else longdiv (r*10) divisor (l-1) (q:ans)
-}

longdiv _ _ 0 rems = length rems
longdiv dd divisor l rems = ans' where
    (q,r) = quotRem dd divisor
    maxl = divisor
    ans' = if r == 0 then (0)
	   else if elem r rems then (maxl-l)
	   else longdiv (r*10) divisor (l-1) (r:rems)

maxcycle maxn = maxcycle' [maxn',maxn'-2..5] (0,0) where
    maxn' = if odd maxn then maxn else maxn-1
    maxcycle' [] (n,l) = (n,l)
    -- cycle can not be > than divisor
    maxcycle' (x:xs) (n,l) = if (l>x) then (n,l) else maxcycle' xs (n',l') where
	l2 = longdiv 1 x maxl []
	maxl = x
	(n',l') = if l2 > l then (x,l2) else (n,l)

main = print $ maxcycle 1000
