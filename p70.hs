import Data.List
import Control.Monad
import Primes


f = do
    let lm = floor $ sqrt $ 10000000
	ps = takeWhile (<100000) primes
    x <- takeWhile (<lm) ps
    y <- dropWhile (<lm) ps
    let n = x*y
	factors = n - quot n x - quot n y + 1
	ratio = fromIntegral n / fromIntegral factors
    guard (n<10000000 && (sort $ show n) == (sort $ show factors))
    return (ratio,n,factors,x,y)

main = print $ minimum f
