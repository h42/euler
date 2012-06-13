import Data.Array

goal = 200
ma = listArray (0,6) [200,100,50,20,10,5,2]
(_,ub) = bounds ma

p31 :: Int -> Int -> Int -> Int -> Int
p31 level i curtot ans
    | newtot < goal  = p31 level (i+1) curtot ans'
    | newtot == goal = ans+1
    | otherwise      = ans
  where newtot = curtot + i * (ma ! level)
	ans' = if level >= ub  then (ans+1)
	       else  p31 (level+1) 0 newtot ans

-- from iogann on forum - pretty but much slower
problem31 = combi 200 [200, 100, 50, 20, 10, 5, 2, 1]
    where
      combi _ []        = 0
      combi 0 _         = 1
      combi n _ | n < 0 = 0
      combi n coins     = combi (n - (head coins)) coins + (combi n (tail coins))

--main = print $ problem31
main = print $ p31 0 0 0 0

