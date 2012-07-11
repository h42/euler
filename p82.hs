import Data.List

zs=transpose
 [[131,673,234,103,18]
 ,[201,96,342,965,150]
 ,[630,803,746,422,111]
 ,[537,699,497,121,956]
 ,[805,732,524,37, 331]]

high =999999999

cost (y:ys) = minimum $ f1 ys y

f1 [] cs = cs
f1 (y:ys) cs = f1 ys a where a = f2 y cs high

f2 [] _ _ = []
f2 xs cs pre = a : f2 (tail xs) (tail cs) a where  a = f3 xs cs pre

f3 xs cs pre = min (head xs + pre) (minimum $ zipWith (+) cs (scanl1 (+) xs))

main = do
    mapM print zs
    putStrLn ""
    print $ f2 (zs!!1) (zs!!0) high
    putStrLn ""
    print $ cost zs

