p39 n = [(n-c-b,b,c,n) | c<-[3..quot n 2 + 1], b<-[quot (n-c) 2..c-1 ],
		   b<c,  c*c == ((n-c-b)*(n-c-b)) + b*b]

main = do
    let ts = map p39 [10..1000]
	ts2 = map (\x->(length x,x)) ts
    print $ maximum ts2
--    print $ p39 1000
--    print $ p39 120
