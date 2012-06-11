import Data.List
import Primes

ps = takeWhile (<10000) $dropWhile (<1000) primes

p49 :: Int -> String
p49 x = if l>2 then ans else "" where
    sx = show x
    p1 = map read (permutations sx)
    p2 = nub $ filter (\p->(p==x||p==x+3330||p==x+6660) && isPrime primes p) p1
    l = length p2
    ans = show x ++ show (x+3330) ++ show (x+6660)

main = print $ filter (not.null) $ map p49 ps

