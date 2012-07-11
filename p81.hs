as = fmap (\s-> map (\x->read ("["++x++"]")) $ lines s) $readFile "p81.txt"

cost2 [] _ psum os = reverse os
cost2 (x:xs) (x1:x1s) psum os = cost2 xs x1s c (c:os) where
    c = min (x+x1) (x+psum)

cost1 [] os = os
cost1 (y:ys) os = cost1 ys os' where
    os' = cost2 y os 999999999 []

cost :: [[Int]] -> Int
cost (y:ys) = ans where
    os = scanl1 (+) y
    ans = last $ cost1 ys os

main = fmap cost as >>= print
