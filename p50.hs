import Primes

n=1000000
maxl = length $ takeWhile (<n) sumlist
plist = takeWhile (<n) primes
sumlist = scanl1 (+) plist

p50a frame = if isPrime plist y
    then (0,0,y) : p50b sumlist ys
    else p50b sumlist ys
  where
    (y:ys) = drop (frame-1) sumlist

p50b _ [] = []
p50b (x:xs) (y:ys)
    | z > n = []
    | otherwise = if isPrime primes z
	then  zz : p50b xs ys
	else  p50b xs ys
  where z = y - x
	zz = (x,y,z)

p50 20 = "Something went wrong"
p50 i =
    case ys of
	[] -> p50 (i-1)
	(_,_,p):_ -> "Prime " ++ show p ++ " is the sum of "
			      ++ show i ++ " primes"
  where ys = p50a i

main = print (p50 $ maxl-1)  -- maxl = 546

