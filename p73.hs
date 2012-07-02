{-# LANGUAGE BangPatterns #-}
import Data.Ratio

fs (a,b) n = maximum $ filter (\x -> x /= a%b) [quot (a*y) b % y | y<-[n-20..n]]

next n (a,b) (c,d) = ((c,d),(p,q)) where
    k = floor (fromIntegral (n+b) / fromIntegral d)
    p = k * c - a
    q = k * d - b

p73 n (numer,denom) end = p73a n end (numer , denom) (1,3) 0

p73a n end (a,b) (c,d) !cnt
    | (c',d') == end = cnt
    | otherwise = p73a n end (a',b') (c',d') cnt'
    where ((a',b'),(c',d')) = next n (a,b) (c,d)
	  cnt' = (if c'==1 && d'==3 then 0 else cnt+1)

main = do
    let n = 12000
    print $ (p73 n (numerator $ fs (1,3) n, denominator $ fs (1,3) n) (1,2))
