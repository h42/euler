
ds = filter (\n -> not $ isSquare 0 n n ) [2..1000]

p66 d
    | isSquare d 0 d = Nothing
    | otherwise = 2

cfracConvs :: [Integer] -> [(Integer,Integer)]
cfracConvs xs = map (\n -> cfracConv $ take n xs) [2..length xs]
cfracConv xs = cfc (tail xs') (head xs') 1 where xs' = reverse xs
cfc [] n d = (n,d)
cfc (x:xs) n d = cfc xs (n * x + d) n


isSquare xlow xhigh y2
    | xhigh - xlow < 2 = False
    | x2 < y2 = isSquare x xhigh y2
    | x2 > y2 = isSquare xlow x y2
    | otherwise = True
  where
    x = quot (xhigh + xlow) 2
    x2 = x * x

main = do
    print $ take 10 ds
    let cf = [1,2,2,2,2,2,2,2,2,2]
    print $ cfracConvs  cf
