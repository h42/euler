import Data.Char

isTri n = if floor r == ceiling r then True else False where
    r = (sqrt (1.0 + 8 * fromIntegral n) - 1) / 2

p42 sx = sum $ map (\c->ord c - ord 'A' + 1) (filter isAlpha sx)

main = do
    sx <- readFile "words.txt"
    print $ length $ filter (isTri.p42) (read sx :: [String])
