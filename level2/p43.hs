import Data.List
import Euler

panlist = permutations [0..9]
ids   = [(1,2),(2,3),(3,5),(4,7),(5,11),(6,13),(7,17)]

chkpan pandy = all chkpan2 ids where
    chkpan2 (i,p) = if rem n p == 0 then True else False  where
	n = list2num $ take 3 (drop i pandy)

main = print $ sum $ map list2num $ filter chkpan panlist

