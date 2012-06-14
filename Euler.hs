module Euler (
    ilen
    ,digitN
    ,triList
    ,elemTriList
    ,isTri
    ,num2list
    ,list2num
    ,fac
    ,comb
) where

-----------------------------
-- NUM2LIST / LIST2NUM
-----------------------------
num2list n = num2list' n []
num2list' 0 xs = xs
num2list' n xs = num2list' q (r:xs) where (q,r) = quotRem n 10
list2num :: Num a => [a] -> a
list2num xs = foldl (\a x->a*10+x) 0 xs  -- works for positive numbers

-----------------------------
-- ILEN
-----------------------------
ilen 0 = 0
ilen x
    | x2==0 = 0
    | x2<10 = 1
    | x2<100 = 2
    | x2<1000 = 3
    | x2<10000 = 4
    | x2<100000 = 5
    | otherwise =ilen' (quot x2 100000) 5
  where x2 = abs x

ilen' 0 a = a
ilen' x a = ilen' (quot x 10) (a+1)

-----------------------------
-- ILEN
-----------------------------
digitN x i
    | i >= l = -1
    | otherwise = d
  where
    l = ilen x
    d = rem (quot x (10^((l-i)-1))) 10



-----------------------------
-- TRIANGLE NUMBERS
-----------------------------
triList = (1:zipWith (+) triList [2..] :: [Int])
elemTriList n = elem n (takeWhile (<(n+1)) triList)
isTri n = if floor r == ceiling r then True else False where
    r = (sqrt (1.0 + 8 * fromIntegral n) - 1) / 2

-----------------------------
-- COMBINATORICS
-----------------------------
fac n
    | n>1 = product [2..n]
    | otherwise = 1

comb n r
    | n>r = product [n,n-1..n-r+1] `quot` (fac r)
    | otherwise = 1

