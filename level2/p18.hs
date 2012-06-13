toInts [] intlist [] = reverse intlist
toInts acc intlist [] = reverse $ (read $ reverse acc :: Int):intlist
toInts acc intlist (' ':xs)  =
    if acc == "" then toInts acc intlist xs
	       else toInts [] ((read $ reverse acc):intlist) xs
toInts acc intlist (x:xs) = toInts (x:acc) intlist xs

doit :: [[Int]] ->  [Int] -> Int
doit [] bs = head bs -- should only be 1 element in bs
doit (x:xs) bs = doit xs bs2
    where bs2 = getbig x bs []

getbig :: [Int] -> [Int] -> [Int] -> [Int]
getbig [] _ ans = reverse ans
getbig (x:xs) (b1:b2:bs) ans  = getbig xs (b2:bs) (x + max b1 b2 : ans)


main = do
    -- let d' = ["3","7 4","2 4 6","8 5 9 3"]
    d' <- fmap lines $ readFile "p18.dat"
    let d = reverse $ map (toInts [] []) d'
	ans = doit (tail d) (head d)
    --mapM print d
    print ans

