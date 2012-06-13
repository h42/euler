
sumd5 n 0 t = (t == n,n,t)
sumd5 n x t = sumd5 n q t' where
    (q,r) = quotRem x 10
    t' = t + r*r*r*r*r

main = do
    let y1 = map (\i->sumd5 i i 0) [11..999999]
	y2 = filter (\(good,n,t)->good==True) y1
	ys  = map (\(good,n,t)->n) y2
    mapM print ys
    print $ sum ys
