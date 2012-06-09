int2bin 0 = []
int2bin x = r : int2bin q where (q,r) = quotRem x 2

p36 x = xs == reverse xs && bs == reverse bs where
    xs = show x
    bs = int2bin x

main = print $ sum $ filter p36 [1..999999]

