import qualified Data.IntSet as S

abundant x = factorSum x > x

abundants = filter abundant [2..upperLimit]

--upperLimit = 28123
upperLimit = 20162

factorSum :: Int -> Int
factorSum n
    | n<2 = 0
    | otherwise = fact' 2 1 where

  fact' i fs = ans where
	(q,r) = quotRem n i
	ans = if i^2 > n then fs
	      else if r /= 0 then fact' (i+1) fs
	      else if i == q then i+fs
	      else fact' (i+1) (i+q+fs)

main = do
    print $ length abundants

    let sums = [x+y | x<-abundants, y<-abundants, x<=y, x+y<=upperLimit]
	set = S.fromList sums
	nogo = filter (\x-> S.notMember x set) [1..upperLimit]
    print $ S.size set
    print $ length nogo
    print $ sum nogo

