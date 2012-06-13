import Data.Array

fa = listArray (0,9) (1:[fac x | x<-[1..9]])
fac n = product [1..n]

p34 :: Int -> Bool
p34 n = n == p34' n

p34' 0 = 0
p34' n =  (fa ! r) + p34' q where
    (q,r) = quotRem n 10


main = do
    --print $ p34 145
    print $ sum $ filter p34 [3..2600000]
