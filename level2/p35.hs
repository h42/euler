import qualified Data.Set as S
import Primes

zn = 1000000
pset = S.fromList $ takeWhile (<zn) primes

p35 x
    | S.notMember x pset = False
    | x < 10 = True
    | x < 100 = p35' x 1 1
    | x < 1000 = p35' x 2 2
    | x < 10000 = p35' x 3 3
    | x < 100000 = p35' x 4 4
    | x < 1000000 = p35' x 5 5

p35' x 0 e = True
p35' x n e = if bad then False else p35' y (n-1) e
    where y = rotateInt x e
	  bad = S.notMember y pset

main = do
   let cps = filter p35 (2:[3,5..zn])
   print cps
   print $ length cps

