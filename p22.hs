import Data.List
import Data.Char

main = do
    osx <- readFile "p22.txt"
    let sx = zip [1..] (sort $ read osx :: [String])
    print $ foldl (+) 0 (map prec sx)

prec (i,name) = (nameval name) * i

nameval name = sum $ map (\c->ord c - ord 'A' + 1) name

