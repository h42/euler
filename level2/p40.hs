import Data.Char (digitToInt)
getnd n =  digitToInt $ concatMap show [1..] !! (n-1)
main = print $ product $ map getnd [1,10,100,1000,10000,100000,1000000]
