{-# LANGUAGE BangPatterns #-}
import Primes
import qualified Data.Set as S
import Data.List

n = 9999
psx   = (takeWhile (<n) primes) :: [Int]
maxp = maximum psx
lx2 = [(x,y)|x<-psx,y<-psx,x>2,y>x,memp (show x) (show y)]

pset = S.fromList lx2
ps2 = sort $ nub $ foldl (\a (x,y)-> x:y:a) [] lx2

memp x y = isPrime primes (read (x++y)) &&  isPrime primes (read (y++x))

mem x y = S.member (min x y,max x y) pset

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
    -- | mem a d && mem b d && mem c d = (a,b,c,d)
    | otherwise          = chk3 aas bbs ccs ds

chk4 aas@(a:as) bbs@(b:bs) ccs@(c:cs) (d:ds) [] = chk3 aas bbs ccs ds
chk4 aas@(a:as) bbs@(b:bs) ccs@(c:cs) dds@(d:ds) (e:es)
    -- | True = (a,b,c,d,"")
    | mem a e && mem b e && mem c e && mem d e = (a,b,c,d,e)
    | otherwise          = chk4 aas bbs ccs dds es

main = do
    print $ take 10 ps2

    let (a1:as) = ps2
    let (a,b,c,d,e) = chk1 (a1:as) as
    print $ (a,b,c,d,e)
    print $ a+b+c+d+e
