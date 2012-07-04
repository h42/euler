import qualified Data.IntMap as M
import Data.List
import Control.Monad

p = 1500000 -- perimeter

pts = do
    n <- [2..1000]
    m <- [1..n-1]
    guard (odd (n-m) && gcd n m == 1) -- insure primitive
    let a = n^2 - m^2
	b = 2 * n * m
	c = n^2 + m^2
	l = a + b + c
    guard (l<=p)
    return (l,n,m,a,b,c) -- only need l for p075

loadmap [] m = m
loadmap ((l,_,_,_,_,_):xs) m = loadmap xs (M.insertWith (+) l 1 m)

chkmap k m = case (M.lookup k m) of Just x -> x ; _ -> 0

sieve m [] = m
sieve m (x:xs) = sieve (sieve' m x 2) xs
sieve' m x i
    | x*i < p = sieve' (M.insertWith (+) (x*i) 1 m) x (i+1)
    | otherwise = m

main = do
    let m = loadmap pts M.empty
    let m2 =  sieve m (M.keys m)
    print $ length $ filter (\x->chkmap x m2 == 1) [12..p]
