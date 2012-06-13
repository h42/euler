import Primes

chkPattern :: String -> (Int,String,[String])
chkPattern pat = (f3,pat,nums) where
    nums = filter ((/='0').head) $ map (appWild pat) ['0'..'9']
    f3 = foldl (\a x -> if isPrime primes (read x) then 1+a else a) 0 nums

appWild :: String -> Char -> String
appWild [] _ = []
appWild (t:ts) x = (if t=='*' then x else t) : appWild ts x

 -- should check for n==p
mkPattern n ps xs = mkPattern0 n (map (\x->n-x+1)  ps) xs
mkPattern0 n (p:ps) [] = '*' : mkPattern0 (n-1) ps []
mkPattern0 n [] (x:xs) = x : mkPattern0 (n-1) [] xs
mkPattern0 n pps@(p:ps) xxs@(x:xs) =
    if p==n then '*' : mkPattern0 (n-1) ps xxs
    else x : mkPattern0 (n-1) pps xs
mkPattern0 _ _ _ = []

getMax n  = ans where
    wilds = [ [i,j,k] | i<-[1..n-2], j<-[i+1..n-1], k<-[j+1,n] ]
    ms ws = map (chkPattern.(mkPattern n ws).show) [10^(n-3)..10^(n-2) -1]
    ans = maximum $ concatMap ms wilds

main = print $ getMax 5
