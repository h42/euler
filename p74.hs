import Data.Char
import Data.List

facnum n = sum $ map (\x->product [1..digitToInt x]) (show n)

chknum n = (notElem (last xs') (init xs') || notElem (last xs) (init xs) ,n) where
    xs = take 60 $ iterate facnum n
    xs' = take 10 xs

main = print $ length $ filter fst $ map chknum [2..999999]
