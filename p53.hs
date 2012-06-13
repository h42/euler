fac n
    | n>1 = product [2..n]
    | otherwise = 1

comb n r = fac n `quot` (fac r * fac (n-r))

main = do
    print $ length $
	filter (>1000000) [comb n r | n<-[1..100], r<-[1..n]]

