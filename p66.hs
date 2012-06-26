import Data.List

ds = [2..1000] \\ (map (^2) [2..33])

cfracConvs :: [Integer] -> [(Integer,Integer)]
cfracConvs xs = map (\n -> cfracConv $ take n xs) [2..length xs]
cfracConv xs = cfc (tail xs') (head xs') 1 where xs' = reverse xs
cfc [] n d = (n,d)
cfc (x:xs) n d = cfc xs (n * x + d) n

-- cfracSqrtLong expands the cycle to give infinite list
cfracSqrtLong n = if l == 1 then cfl else head cfl:(cycle $ tail cfl) where
    cfl = cfracSqrt n
    l = length cfl

cfracSqrt n = if rn ^ 2 == n  then  [rn]
	      else doit (0) (1) [rn] (-1,-1,-1)  where
    rn = floor $ sqrt $ fromIntegral n
    
    doit c d ans (ca,cc,cd) = if (a' == ca && c' == cc && d' == cd)
	then reverse $ if length ans' == 1 then ans' else tail ans'
	else doit c' d' ans' (ca',cc',cd')
      where
	a = head ans
	c' = - (c - a * d)
	d' = (n - c'^2) `quot` d
	a' = d * (rn  + c') `quot` (n - c'^2)
	ans' = a' : ans
	(ca',cc',cd') = if ca<0 then (a',c',d') else (ca,cc,cd)

p66b ((x,y):cs) d = if x*x - d* y * y == 1 then Just (x,y,d) else p66b cs d
p66b [] _ = Nothing

p66 d = p66b (cfracConvs $ take 100 $ cfracSqrtLong d) d

main = print $ maximum $  map p66 $ takeWhile (<=1000) ds
