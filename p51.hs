import Primes

infixl 7 %
(%) = mod

-- apply value to template
appWild :: String -> Char -> String
appWild [] _ = []
appWild (t:ts) x = (if t=='*' then x else t) : appWild ts x

chkPattern :: String -> Int
chkPattern pat = f3 where
    nums = filter ((/='0').head) $ map (appWild pat) ['0'..'9']
    f3 = foldl (\a x -> if isPrime primes (read x) then 1+a else a) 0 nums

 -- should check for n==p
mkPattern n ps xs = mkPattern0 n (map (\x->n-x+1)  ps) xs
mkPattern0 n (p:ps) [] = '*' : mkPattern0 (n-1) ps []
mkPattern0 n [] (x:xs) = x : mkPattern0 (n-1) [] xs
mkPattern0 n pps@(p:ps) xxs@(x:xs) =
    if p==n then '*' : mkPattern0 (n-1) ps xxs
    else x : mkPattern0 (n-1) pps xs
mkPattern0 _ _ _ = []

main = do
    print $ chkPattern "*3"
    print $ chkPattern "56**3"
    print $ mkPattern 5 [3,4] "563"
    print $ mkPattern 5 [1,2] "563"
    print $ mkPattern 5 [4,5] "563"

