import Data.Array

d1 = listArray (1,19)
	 ["one","two","three","four","five","six","seven","eight","nine","ten"
	  ,"eleven","twelve","thirteen","fourteen","fifteen"
	  ,"sixteen","seventeen","eighteen","nineteen"]
d2 = listArray (2,9) ["twenty","thirty","forty","fifty","sixty","seventy"
		       ,"eighty","ninety"]

int2text x
    | x <= 0 = "bad"
    | x < 20 = d1 ! x
    | x < 100 = if r == 0 then d2 ! q  else (d2 ! q) ++ "-" ++ (d1 ! r)
    | x < 1000 = d1 ! q2 ++ " hundred " ++
	(if r2>0 then ("and " ++ int2text r2)  else " ")
    | otherwise       = "one thousand"
  where (q,r) = quotRem x 10
	(q2,r2) = quotRem x 100

main = do
    print $ length $ filter (\x->if x /= ' ' && x/= '-' then True else False)
		 $ concat $ map int2text [1..1000]
    --mapM_ putStrLn (map int2text [20,30,42,99,107,242,999,1000])
