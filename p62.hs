import Data.Char
import Data.List

sums :: [ ([Char], Int) ]
sums = head $ filter (\x->length x>=5) $
		  groupBy (\x y-> fst x == fst y) $
		  sort $ map (\x->(sort (show (x^3)),x^3)) [1000..9999]

main = mapM print sums
