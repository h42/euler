import Data.Array
-- p(k) = p(k − 1) + p(k − 2) − p(k − 5) − p(k − 7) + p(k − 12) +
--        p(k − 15) − p(k − 22) + ...
-- p(0) = 1 ; p(<0) = 0

penta = map (\k->floor $ k * (3*k - 1)/2) $ concatMap (\x->[x,-x]) [1.0,2.0..]
neg2 = repeat [False,False,True,True]

partfunc n = f zts 0 0    where
    zts = takeWhile (<=n) penta
    f [] _ ans =  ans
    f (t:ts) cnt ans = f ts cnt' ans' where
	cnt' = if cnt>=3 then 0 else cnt+1
	y = parta ! (n-t)
	ans' = if cnt<2 then ans+y else ans-y

bign=100000
parta=listArray (0,bign) $ 1 : (take bign $ map (partfunc) [1..])

main =
  print $ head $ filter (\(x,y)->rem y 1000000 ==0) $ zip [0..] $ elems parta
