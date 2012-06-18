import Data.Char
main = do
    print $ length s3
    print $ length s4
    print $ length s5
    print $ length s6
    let ans =  solve s3 s4 s5 s6
    mapM print ans

s3 = fsum $ takeWhile (<10000) $ dropWhile (<1000) $ map (\n->quot (n*(n+1)) 2) [2..]
s4 = fsum $ takeWhile (<10000) $ dropWhile (<1000) $ map (\n->n*n) [2..]
s5 = fsum $ takeWhile (<10000) $ dropWhile (<1000) $ map (\n->quot (n*(3*n-1)) 2 ) [2..]
s6 = fsum $ takeWhile (<10000) $ dropWhile (<1000) $ map (\n->n*(2*n-1)) [2..]
s7 = fsum $ takeWhile (<10000) $ dropWhile (<1000) $ map (\n->quot (n*(5*n-3)) 2) [2..]
s8 = fsum $ takeWhile (<10000) $ dropWhile (<1000) $ map (\n->n*(3*n-2)) [2..]

conv x = (q,r,x) where (q,r) = quotRem x 100
fsum sl = map conv sl

solve l1 l2 l3 l4 = do
    a<-l1
    let m = comp a l2
    b<-m
    let n = comp b l3
    c<-n
    let o = comp2 a c l4
    d<-o
    return (a,b,c,d)

comp (xl,xr,xn) = filter (\(l,r,n) -> xr == l)
comp2 (al,ar,an) (xl,xr,xn) = filter (\(l,r,n) -> xr == l && al == r)
