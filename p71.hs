import Data.Ratio
import Data.List

fs = filter (\x -> x /= 3%7 && 3%7 - x < 1%2000000)
    [quot (3*y) 7 % y | y<-[100000..1000000]]
main = print $ head $ reverse $ sort fs

