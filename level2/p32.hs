import Data.List

pandit n1 n2 n3 = ans where
    ds = show n1 ++ show n2 ++ show n3
    ans = if elem '0' ds then False
	  else if (length $ nub ds) == 9 then True
	  else False

chklens n1 n2 n3 = chklen n1 + chklen n2 + chklen n3

chklen :: Int -> Int
chklen n1
    | n1 < 10 = 1
    | n1 < 100 = 2
    | n1 < 1000 = 3
    | n1 < 10000 = 4
    | otherwise = 5

gen i j a
    | l > 9 = if chklens i1 i1 (i1*i1) > 9 then a else gen i1 i1 a
    | l == 9 = if pandit i j (i*j) then gen i (j+1) (i*j:a)
				   else gen i (j+1) a
    | otherwise = gen i (j+1) a
  where l = chklens i j (i*j)
	i1 = i+1

main = do
    let ans = sort $ gen 3 3 []
    mapM print ans
    print $ sum ans
    print $ sum $ nub ans

