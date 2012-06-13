n=quot 1000 2

ans :: [(Int,Int,Int,Int)]
ans=[(a*b*c,a,b,c) | c<-[1..(n-3)], a<-[1..c], b<-[1..c], a<=b, a^2 + b^2 == c^2,
	       a+b+c==1000]

main = do
    print ans

