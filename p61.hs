import Data.Char
import Data.List

main = print $ filter (not.null)
	    $ map solve6 $ map ([s3]++) (permutations [s4,s5,s6,s7,s8])

s3 = getvals $ map (\n->quot (n*(n+1)) 2) [2..]
s4 = getvals $ map (\n->n*n) [2..]
s5 = getvals $ map (\n->quot (n*(3*n-1)) 2 ) [2..]
s6 = getvals $ map (\n->n*(2*n-1)) [2..]
s7 = getvals $ map (\n->quot (n*(5*n-3)) 2) [2..]
s8 = getvals $ map (\n->n*(3*n-2)) [2..]

getvals xs = takeWhile (<10000) $ dropWhile (<1000) xs

solve6 [l1,l2,l3,l4,l5,l6] = do
    a<-l1
    let m = comp a l2
    b<-m
    let n = comp b l3
    c<-n
    let o = comp c l4
    d<-o
    let p = comp d l5
    e<-p
    let q = comp2 a e l6
    f<-q
    return (a + b + c + d + e + f)

comp x = filter (\l -> rem x 100 == quot l 100)
comp2 a x = filter (\l -> rem x 100 == quot l 100  && quot a 100 == rem l 100)
