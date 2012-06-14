fac n
    | n>1 = product [2..n]
    | otherwise = 1

comb n r
    | n>r = product [n,n-1..n-r+1] `quot` (fac r)
    | otherwise = 1

main = do
    print $ length $
	filter (>1000000) [comb n r | n<-[1..100], r<-[1..n]]

