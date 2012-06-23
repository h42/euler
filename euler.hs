import Euler

set = [1..5]
n = comb (length set) 3

combinations = do
    a<-set
    b<-dropWhile (<=a) $tail set
    return [a,b]

main = do
    print combinations

main2 = do
    putStrLn $ show 117 ++ " - " ++ show (ilen 117)
    putStrLn $ show 1234567 ++ " - " ++ show (ilen 1234567)
    print $ digitN 1234567 4
    mapM print (map (digitN 1234567) [0..7])

