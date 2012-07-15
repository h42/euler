import Data.Char

p65 xs = p65a (tail xs) (head xs) 1
p65a [] n d = (n,d)
p65a (x:xs) n d = p65a xs (n*x+d) n

e = reverse $ take 100 $ 2: concat [[1,x,1] | x<-[2,4..]]

main = print $ sum $ map digitToInt (show $ fst $ p65 $ e)

