import Data.List

ilen x
    | x<10 = 1
    | x<100 = 2
    | x<1000 = 3
    | x<10000 = 4
    | x<100000 = 5
    | otherwise = error "ilen number out of range"

pandit ds = if elem '0' ds  then False
	    else if (length $ nub ds) == 9 then True
	    else False

p38 n = if len==9 && pandit ds then (n,ds) else (0,"")  where
    ys = p38a n 1 0
    f1 = foldl (\(ay,al) (y,l) -> (ay++show y,l)) ([],0) ys
    (ds,len) = f1

p38a n ind tl = if tl' < 10  then (y,tl') : p38a n (ind+1) tl'   else []
    where y   = n * ind
	  tl' = ilen y + tl

main = do
    let pds = filter (\(n,ds)->n/=0) (map p38 [1..9999])
    mapM print pds
    putStrLn $ "max = " ++ (show $maximum pds)
