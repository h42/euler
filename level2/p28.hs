
main = print $ (sum l1) + sum (take n l2) - 1

n=1001
l1 = 1: zipWith (+) l1 [2,4..(n-1)*2]
l2 = 1: zipWith (+) l2 ( concat [[x,x] | x<-[4,8..]] )
