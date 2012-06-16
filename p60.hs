{-# LANGUAGE BangPatterns #-}
import Primes
import qualified Data.IntSet as S

n = 10000000
n2 = 9999

pset = S.fromList $ takeWhile (<n) primes
psx   = map show (takeWhile (<n2) primes)

mem :: String -> String -> Bool
mem !x !y = S.member (read (x++y)) pset &&  S.member (read (y++x)) pset


nogo = ("","","","","")

chk1 [!a1,!b1] [] = nogo
chk1 (!a1:a2:as) [] = chk1 (a2:as) as
chk1 !aas@(!a:as) !bbs@(!b:bs)
    | mem a b = chk2 aas bbs bs
    | otherwise = chk1 aas bs

chk2 aas@(!a:as) bbs@(!b:bs) [] = chk1 aas bs
chk2 !aas@(!a:as) !bbs@(!b:bs) !ccs@(!c:cs)
    | mem a c && mem b c = chk3 aas bbs ccs cs
    | otherwise          = chk2 aas bbs cs

chk3 aas@(a:as) bbs@(b:bs) ccs@(c:cs) [] = chk2 aas bbs cs
chk3 !aas@(!a:as) !bbs@(!b:bs) !ccs@(!c:cs) !dds@(!d:ds)
    | mem a d && mem b d && mem c d = chk4 aas bbs ccs dds ds
    | otherwise          = chk3 aas bbs ccs ds

chk4 !aas@(a:as) !bbs@(b:bs) !ccs@(c:cs) !(d:ds) ![] = chk3 aas bbs ccs ds
chk4 !aas@(!a:as) !bbs@(!b:bs) !ccs@(!c:cs) !dds@(!d:ds) !(e:es)
    -- | True = (a,b,c,d,"")
    | mem a e && mem b e && mem c e && mem d e = (a,b,c,d,e)
    | otherwise          = chk4 aas bbs ccs dds es

main = do
    --print $ length $ takeWhile (<1000) primes
    print $ length psx
    print $ S.size pset
    let (a1:as) = psx
    print $ chk1 (a1:as) as
