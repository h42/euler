import qualified Data.IntSet as S

ps :: [Int]
ps = [quot (n * (3 * n - 1)) 2 | n <- [1..]]
pset = S.fromList $ takeWhile (<(2*bign)) ps

bign = 10000000

p44 = p44a ps (tail ps) []
p44a (x:xs) yys@(y:ys) ans
    | y > bign = ans
    | x >= y = p44a ps ys ans
    | otherwise =
	if S.member (y+x) pset && S.member (y-x) pset
	    then p44a xs yys ((x,y,y-x):ans)
	    else p44a xs yys ans

main = print $ p44

