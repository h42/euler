
next n (a,b) (c,d) = ((c,d),(p,q)) where
    k = floor (fromIntegral (n+b) / fromIntegral d)
    p = k * c - a
    q = k * d - b

p73 n end = p73a n end (0,1) (1,n) 0

p73a n end (a,b) (c,d) cnt =  if (c',d') == end then cnt
			 else p73a n end (a',b') (c',d') (cnt+1)
    where ((a',b'),(c',d')) = next n (a,b) (c,d)

main = do
    let n = 12000
    print $ (p73 n (1,2)) - p73 n (1,3) - 1
