import Control.Monad
--import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

primesUpto :: Int -> [Int]
primesUpto n = [p | (p, True) <- assocs $ sieve n]

sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
    sieve <- newArray (2, n) True
    forM_ [2..n] $ \p -> do
	isPrime <- readArray sieve p
	when isPrime $ do
	    forM_ [p*2, p*3 .. n] $ \k -> do
		writeArray sieve k False
    return sieve

main = do
    let ps = primesUpto 100000000
    let s = sieve 10
    print $ s

