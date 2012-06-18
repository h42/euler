--
-- glguy's solution - short , sweet and fast
-- (head solve) results in lazy eval winning the day
--

main = print (sum (head solve))

solve = do
 a <- primesTo10000
 let m = f a $ dropWhile (<= a) primesTo10000
 b <- m
 let n = f b $ dropWhile (<= b) m
 c <- n
 let o = f c $ dropWhile (<= c) n
 d <- o
 let p = f d $ dropWhile (<= d) o
 e <- p
 return [a,b,c,d,e]
 where
  f x = filter (\y -> isPrime (read (shows x (show y)))
		&& isPrime (read (shows y (show x))))

primesTo100 :: [Integer]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

trialDivision ps n = doTrialDivision ps
    where doTrialDivision (p:ps) =
	    let (q,r) = n `quotRem` p in
	      if r == 0     then False
	      else if q < p then True
	      else doTrialDivision ps
	  doTrialDivision [] = True

primesTo10000 = primesTo100 ++ filter (trialDivision primesTo100) [101,103..9999]

isTrialDivisionPrime 2 = True -- special case, not caught by above code
isTrialDivisionPrime n = trialDivision (primesTo10000 ++ [10001,10003..]) n

isPrime = isTrialDivisionPrime

{-
--
-- My original solution
--
import Primes
import qualified Data.Set as S
import Data.List

n = 9999
psx   = (takeWhile (<n) primes) :: [Int]
maxp = maximum psx

mem x y = isPrime primes (read (shows x (show y)))
	     && isPrime primes (read (shows y (show x)))

nogo = (-1,-1,-1,-1,-1)

chk1 [a1,b1] [] = nogo
chk1 (a1:a2:as) [] = chk1 (a2:as) as
chk1 aas@(a:as) bbs@(b:bs)
    | mem a b = chk2 aas bbs bs
    | otherwise = chk1 aas bs

chk2 aas@(a:as) bbs@(b:bs) [] = chk1 aas bs
chk2 aas@(a:as) bbs@(b:bs) ccs@(c:cs)
    | mem a c && mem b c = chk3 aas bbs ccs cs
    | otherwise          = chk2 aas bbs cs

chk3 aas@(a:as) bbs@(b:bs) ccs@(c:cs) [] = chk2 aas bbs cs
chk3 aas@(a:as) bbs@(b:bs) ccs@(c:cs) dds@(d:ds)
    | mem a d && mem b d && mem c d = chk4 aas bbs ccs dds ds
    | otherwise          = chk3 aas bbs ccs ds

chk4 aas@(a:as) bbs@(b:bs) ccs@(c:cs) (d:ds) [] = chk3 aas bbs ccs ds
chk4 aas@(a:as) bbs@(b:bs) ccs@(c:cs) dds@(d:ds) (e:es)
    | mem a e && mem b e && mem c e && mem d e = (a,b,c,d,e)
    | otherwise          = chk4 aas bbs ccs dds es

main = do
    let (a,b,c,d,e) = chk1 psx (tail psx)
    print $ (a,b,c,d,e)
    print $ a+b+c+d+e
-}
