--ps = [rem (x^x) (10^10)|x<-[1..]]
ps = [(x^x)|x<-[1..]]

p48 = mod y (10^10)
    where y = sum $ take 1000 ps

main = do
    print $ p48
    --print $ p48a

