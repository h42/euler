millerRabin :: Integer -> Bool
millerRabin n
    | n == 2          = True
    | n < 2 || even n = False
    | otherwise       =
	let (s,d) = splitWith (n-1) 2
	in all (\a -> modPow a d n == 1 || any (\r -> modPow a (d*2^r) n == n-1) [0..s-1])
	       $ filter (< n) [2,3,5,7,11]

splitWith :: Integer -> Integer -> (Integer,Integer)
splitWith n p = rec 0 n where
    rec s t
	| t `mod` p == 0 = rec (s+1) (t `div` p)
	| otherwise      = (s, t)

modPow :: Integer -> Integer -> Integer -> Integer
modPow _ 0 _ = 1
modPow x n m = f x (n-1) x  where
    f _ 0 y = y
    f a d y = g a d  where
	g b i
	    | even i    = g ((b*b) `mod` m) (i `quot` 2)
	    | otherwise = f b (i-1) ((b*y) `mod` m)

