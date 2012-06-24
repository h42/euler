maxn = 10000
maxcnt = maxn + 1

step1 :: Int -> [Int]
step1 n = ans where
    rn = floor $ sqrt $ fromIntegral n
    ans = if rn ^ 2 == n  then  [rn]
	  else step2 n rn (0) (1) [rn] maxcnt (-1,-1,-1)

step2 _ _ _ _ ans 0 _ = reverse $ if length ans == 1 then ans else tail ans
step2 n rn c d ans cnt (ca,cc,cd) = step2 n rn c' d' ans' cnt' (ca',cc',cd')
  where
    a = head ans
    c' = - (c - a * d)
    d' = (n - c'^2) `quot` d
    a' = d * (rn  + c') `quot` (n - c'^2)
    ans' = a' : ans
    (ca',cc',cd') = if ca<0 then (a',c',d') else (ca,cc,cd)
    cnt' =  if a' == ca && c' == cc && d' == cd  then 0  else cnt - 1

main = do
    print $ length $ filter even $ map (length.step1) [2..maxn]
