import Data.Char
import Data.List

cfsqrt :: Integer -> [Integer]
cfsqrt zx = theans where
    rx = floor $ sqrt $ fromIntegral zx
    theans = if rx ^ 2 == zx  then  [rx]
	  else step2 (0) (1) [rx] 1 (-1,-1,-1)

    step2 _ _ ans 0 _ = reverse $ if length ans == 1 then ans else tail ans
    step2 c d ans cnt (ca,cc,cd) = step2 c' d' ans' cnt' (ca',cc',cd')
      where
	a = head ans
	c' = - (c - a * d)
	d' = (zx - c'^2) `quot` d
	a' = d * (rx  + c') `quot` (zx - c'^2)
	ans' = a' : ans
	(ca',cc',cd') = if ca<0 then (a',c',d') else (ca,cc,cd)
	cnt' =  if a' == ca && c' == cc && d' == cd  then 0  else 1

cfsqrtexp x = head [sn] ++ (cycle $ tail sn)  where sn = cfsqrt x

convergent n xs = conv $ reverse $ take n xs
conv (x:y:xs) = conv2 xs x (x*y+1)
conv xs = if null xs then (0,0) else (head xs,1)
conv2 [] d n = (n,d)
conv2 (x:xs) n d = conv2 xs d (d*x+n)

e = 2 : concat [[1,x,1] | x<-[2,4..]]

--longdiv :: Integer -> Integer -> Integer -> String
longdiv num den cnt = ans where
    (q,r) = quotRem num den
    ans = if r==0 then show q
	  else show q ++ "." ++ zeroes ++ show a
    (a,z) = ldiver r den 0 (1,0) cnt
    zeroes = if z== 0 then ""
	     else replicate z '0'

ldiver _ _ ans (zf,z) 0 = (ans,z)
ldiver num den ans (zf,z) cnt = ans' where
    (q,r) = quotRem (num*10) den
    ans' = if r==0 then (ans * 10 + q,z)
	   else   ldiver r den (ans * 10 + q) (zf',z') (cnt-1)
    zf' = if q /= 0 then 0 else zf
    z' = if zf == 1 && q == 0 then z+1 else z

p80 n x = ans where
    (numerator,divisor) = convergent (2*n) $ cfsqrtexp x
    sx = longdiv numerator divisor (n-1)
    ds = [head sx] ++ (drop 2 sx)
    ans = foldl (\a x'->a + digitToInt x') 0 ds

main = do
    print $ convergent 5 $ cfsqrtexp 2
    print $ convergent 10 e  -- check with p65
    print $ p80 100 2
    print $ sum $ map (p80 100) $ [1..100] \\ [x^2|x<-[1..10]]
