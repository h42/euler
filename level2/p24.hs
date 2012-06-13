--
-- EULERS METHOD
--
fact n = if n>0 then product [2..n] else 0

lexperm 1 r xs ys  = (r, xs, reverse $ head xs:ys)
lexperm n r xs ys  = lexperm (n-1) r' xs' ys' where
    (q,r') = quotRem r (fact (n-1))
    xs' = take q xs ++ drop (q+1) xs
    ys' = (xs !! q) : ys

main = do
    print $ lexperm 10 999999 [0..9] []
    --print $ perms 999999 [0..9]

--
-- BRUTE FORCE ALG
--
perm ps = perm2 ps 0 (-1) (head ps) where
    perm2 [x] _ p pc
	| p < 0 = ps
	| otherwise = perm3 (tail ps2) (p+2) p pc (p+1) (head ps2)
      where ps2 = drop (p+1) ps

    perm2 (x:y:xs) i p pc = perm2 (y:xs) (i+1) p2 pc2 where
	(p2,pc2) = if x < y then (i,x) else (p,pc)

    perm3 [] i p pc q qc = perm4 p pc q qc
    perm3 (x:xs) i p pc q qc = perm3 xs (i+1) p pc q2 qc2 where
	(q2,qc2) = if pc < x then (i,x) else (q,qc)

    perm4 p pc q qc = --trace ("hey " ++ show (p,pc,q,qc))$
       l1 ++ [qc] ++ reverse (l2 ++ [pc] ++ l3)where
	l1 = take p  ps
	l2 = take (q-p-1) (drop (p+1) ps)
	l3 = drop (q+1) ps

perms 0 p = p
perms i p = perms (i-1) p'
    where p' = perm p

