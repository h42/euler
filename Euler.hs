module Euler (
    ilen
    ,digitN
) where

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
