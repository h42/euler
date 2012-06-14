import Data.List
import Data.Maybe

main = do
    hs <- fmap lines  $readFile "poker.txt"
    {-print $ phand $ "AD KD QD TD JD  KD QD JD TD 9D"
    print $ phand $ "3H 3D 3D 3D QD  3D 3D 3D TD TC"
    print $ phand $ "3D 6D 9D 3D QD  2D 3D 4D 5D 6C"
    print $ phand $ "3D 6D 9D 3S 3D  4D 3D 4D 5D 5C"
    print $ phand $ "3D 6D 9D 3S AD  2D 3D 4D 5D QC"
    print $ phand $ "3D 6D 9D 3S JD  3D 6D 9D 3S AD"
    print $ phand $ "3D 6D 9D 3S JD  3D 6D 9D 3S TD"-}
    print $ length $ filter phand hs

phand hs = win where -- (win,(x1,y1),(x2,y2)) where
    (x1,y1) = phand2 $ map (\[r,s]->(r,s)) (take 5 ws)
    (x2,y2) = phand2 $ map (\[r,s]->(r,s)) (drop 5 ws)
    win = if x1 > x2 || x1 == x2 && y1>y2 then True else False
    ws = words hs

phand2 cards = (score,groups) where
    score2 = (ranks,flush)
    suits = map snd cards
    ranks = sortBy (flip compare) $ map (tr.fst) cards
    groups = sortBy mysort (group ranks)
    mysort a b = if l1 /= l2 then compare l2 l1 else compare b a
	where l1 = length a ; l2 = length b
    maxr = head ranks
    minr = last ranks
    flush = 1 == length (nub suits)
    straight = maxr - minr == 4 && length groups==5
    score
	| maxr==12 && straight && flush = 10  -- royal straight flush
	| straight && flush             = 9   -- straight flush
	| length (head groups) == 4     = 8   -- 4 of a kind
	| length groups == 2            = 7   -- full house
	| flush                         = 6
	| straight                      = 5
	| length (head groups) == 3     = 4   -- 3 of a kind
	| length (head groups) == 2 && length groups == 3 = 3   -- 2 pair
	| length (head groups) == 2     = 2   -- 1 pair
	| otherwise                     = 1


tr :: Char -> Int
tr c = fromJust $elemIndex c (['2'..'9']++['T','J','Q','K','A'])

