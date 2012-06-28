import Data.List
import Data.Maybe

-- magig 5gon

fl = filter ff $ permutations [1..10] where
    ff [a,b,c,d,e,f,g,h,i,j] =
	a+b+c == d+c+e &&
	a+b+c == f+e+g &&
	a+b+c == h+g+i &&
	a+b+c == j+i+b

tl =  map disp (map tf fl) where
    tf [a,b,c,d,e,f,g,h,i,j] = [(a,b,c),(d,c,e),(f,e,g),(h,g,i),(j,i,b)]

disp xs = showl ys where
    ei = minimum xs
    i = fromJust $ elemIndex ei xs
    ys = (take (5-i) $ drop i xs) ++ take i xs

    showl [x1,x2,x3,x4,x5] = show2 x1 ++ show2 x2 ++ show2 x3
				      ++ show2 x4 ++ show2 x5
	where show2 (a,b,c) = show a ++ show b ++ show c

main = print $ last $ nub $ sort $ filter (\x->length x == 16) tl
