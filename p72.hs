import Primes

dfactors zn = dfactors' primes [] zn where
    dfactors' pps@(p:ps) as n
	| n==1 = as
	| p*p > zn = if n /= zn then n:as else as
	| otherwise = dfactors' ps' as' n'
       where
	    (q,r) = quotRem n p
	    a = if null as then 1 else head as
	    as' = if r==0 && a /= p then p:as else as
	    n' = if r==0 then q else n
	    ps' = if r==0 then pps else ps

totient n = if not (null zfs) then tot zfs n else n-1 where
    zfs = dfactors n
    tot [] ans = ans
    tot (f:fs) ans = tot fs (quot (ans * (f-1)) f)

farey_sum n = sum $ map totient [2..n] -- farey sequence = |F(n)| = |F(n-1)| + totient n

main = print $ farey_sum 1000000
