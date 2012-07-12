import Data.List

cost (y:ys) = minimum $ f1 ys y

f1 [] cs = cs
f1 (y:ys) cs = f1 ys a where a = f2 y cs 999999999

f2 [] _ _ = []
f2 xs cs pre = a : f2 (tail xs) (tail cs) a where  a = f3 xs cs pre

f3 xs cs pre = min (head xs + pre) (minimum $ zipWith (+) cs (scanl1 (+) xs))

main = do
    zs <- fmap (\s-> map (\x->read ("["++x++"]")) $ lines s) $ readFile "p82.txt"
    print $  cost (transpose zs)

