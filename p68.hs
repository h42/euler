import Data.List

ml = permutations [1..10]

fl = filter ff $ permutations [1..10] where
    ff [a,b,c,d,e,f,g,h,i,j] =
	a+b+c == d+c+e &&
	a+b+c == f+e+g &&
	a+b+c == h+g+i &&
	a+b+c == j+i+b

tl = map tf fl where
    tf [a,b,c,d,e,f,g,h,i,j] = [(a,b,c),(d,c,e),(f,e,g),(h,g,i),(j,i,b)]

main = do
    print $ length ml
    print $ length fl
    mapM print $ take 10 tl

