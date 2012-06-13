import Data.List

-- idea from forum
main = print $ take 1 $
  [x|x<-[1..179999], all (\y->(sort.show) x == (sort.show) y) [x*i|i<-[2..6]] ]

