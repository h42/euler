n = 100
v1 = sum [x^2 | x <- [1..n]] :: Int
v2 = (sum [1..n]) ^ 2

main = putStrLn $ "sum of squares = " ++ show v1 ++ "\n"    ++
		  "square of sums = " ++ show v2 ++ "\n"    ++
		  "diff           = " ++ show (v2 - v1) ++ "\n"

