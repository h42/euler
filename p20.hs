import Data.Char

main = print $ sum $ map digitToInt (show $ product [2..100])
