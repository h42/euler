import Data.List
import Control.Monad
import Primes

comp x y = (sort $ show x) == (sort $ show y)

phi x y n = n - qx - qy + 1  where
    qx = quot n x
    qy = quot n y

quess = 1.1001389239706996 -- from example

f plow phigh = do
    x <- plow
    y <- phigh
    let n = x*y
    guard (n<10000000)
    let factors = phi x y n
	ratio = fromIntegral n / fromIntegral factors
    guard (ratio < quess && comp n factors)
    return (ratio,n,factors)

main = do
    let ps = takeWhile (<1000000) primes
	plow = takeWhile (<3163) ps
	phigh = dropWhile (<3163) ps
    print $ minimum $ f plow phigh
