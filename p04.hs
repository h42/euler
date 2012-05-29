checker x = sx == reverse sx where sx = show x
pal = maximum [x*y | x<-[100..999], y<-[100..999], checker (x*y)] :: Int

ns' = [x*y | x<-[100..999], y<-[100..999], rem (x*y) 11 == 0] :: [Int]
ploop [] a = a
ploop (n:ns) a = ploop ns (if n>a && checker n then n else a)

-- second solution is much faster but rem 11 only works for 6 digit product

main = print $ ploop ns' 0





