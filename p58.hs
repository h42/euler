import Primes

p58 l diags dprimes
    | l>10 && quot (dprimes*10) (diags-4+1) < 1  = (l-2) -- ,diags-4+1,dprimes)
    | otherwise = p58 (l+2) (diags+4) (dprimes+pcnt l)

pcnt l = length $ filter (isPrime primes) (plist l)
plist sidelen = [ sidelen^2 - (sidelen -1)*x | x<-[1..3]]

main = print $ p58 1 0 0

