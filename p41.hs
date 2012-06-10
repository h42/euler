import Data.List
import Primes

p1 n = filter (isPrime primes) $ map list2num (permutations [1..n] :: [[Int]])
main = print $ maximum $ concatMap p1 [2..9]

