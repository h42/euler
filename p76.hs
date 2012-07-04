import qualified Data.IntMap as M

n = 100
g i j m
    | j==n+2 = g (i+1) (i+1) m
    | i==n+1 = m M.! n - 1
    | otherwise = g i (j+1) (M.insertWith (+) j (m M.! (j-i)) m)

main = print $ (g 1 1 ( M.singleton 0 1))

{-
p[0] = 1
for i in xrange(1, 100):
    for j in xrange(i, 101):
	    p[j] += p[j - i]
	    print p[100]
-}
{-
n = 100 :: Int
f [] _ _ cnt = cnt+1
f (p:ps) i tot cnt
    | tot' < n = f (p:ps) (i+1) tot cnt'
    | tot' == n = (cnt+1)
    | otherwise = cnt
  where tot' = p*i+tot
	cnt' = f ps 0 tot' cnt
main = print $ f [n-1,n-2..2] 0 0 0
-}


