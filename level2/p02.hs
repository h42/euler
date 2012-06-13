
fib = 1 : 2 : zipWith (+) fib (tail fib)

n = 4000000

main = print $ sum $ filter even (takeWhile (<=n) fib)

