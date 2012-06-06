--
-- nth fib = round $ phi ^ n / sqrt 5
--   phi^n / sqrt 5 > 10^999
--   n * log phi - (log 5) / 2 > 999 (log 10)
--   n * log phi > 999 * (log 10)  +  (log 5) / 2
--   n > (999 * (log 10)  +  (log 5) / 2) / (log phi)
--   n = round ( (999 * (log 10)  +  (log 5) / 2) / (log phi) )
--

phi = (1 + sqrt 5.0) / 2
n  = round ( (999 * log 10 + log 5 / 2) / log phi)

main = do
    print phi
    print $ n

fib = 1 : 1 : zipWith (+) fib (tail fib)
main2 = print $ length  (takeWhile (<10^999) fib) + 1

