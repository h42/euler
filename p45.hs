-- all hexs are tris
psl = [quot (x*(3*x-1)) 2|x<-[1..]]
hsl = [x*(2*x-1) | x<-[1..]]

p45 pps@(p:ps) hhs@(h:hs)
    | p==h && p>40755 = p
    | p<h          = p45 ps  hhs
    | otherwise    = p45 pps hs

main = do
    print $ take 20 hsl
    print $ p45 (tail psl) (tail hsl)

