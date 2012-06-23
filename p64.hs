import Data.Ratio
import Debug.Trace

step1 :: Int -> [Int]
step1 n = ans where
    rn = floor $ sqrt $ fromIntegral n
    ans = if rn ^ 2 == n  then  [rn]
	  else step2 n rn (0) (1) [rn] 15

step2 _ _ _ _ ans 0 = reverse ans
step2 n rn c d ans cnt  = step2 n rn c' d' ans' (cnt-1) where
    a = head ans
    c' = - (c - a * d)
    d' = (n - c'^2) `quot` d
    a' = d * (rn  + c') `quot` (n - c'^2)
    ans' = a' : ans

main = do
    print $ step1 2
    print $ step1 3
    print $ step1 5
    print $ step1 6
    print $ step1 7
    print $ step1 23
    print $ step1 46
