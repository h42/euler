import Data.List
import Primes

pf n = length $ nub $ pf2 n primes
pf2 n pps@(p:ps)
    | n <= 1 = []
    | r == 0 = p : pf2 q pps
    | otherwise = pf2 n ps
  where (q,r) = quotRem n p

p47 i
    | f1==4 && f2==4 && f3==4 && f4==4 = i
    | otherwise          = p47 (i+1)
  where
    f1 = pf i
    f2 = pf (i+1)
    f3 = pf (i+2)
    f4 = pf (i+3)

main = do
    print $ p47 14


