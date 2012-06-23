
f n = length $ filter (\x -> n == (length $ show (x^n))) [1..9]

main = do
    print $ sum $ map f [1..25]
