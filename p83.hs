{-# LANGUAGE BangPatterns  #-}
import Data.Map ((!))
import qualified Data.Map as M

msize = 80

type Jmap = M.Map (Int,Int) (Int,Int)

getmap = do
    s <- readFile "p82.txt"
    let zs = map read $ words $ map (\x->if x==',' then ' ' else x) s
    let zs2 = [(i,j)|i<-[1..msize], j<-[1..msize]]
    return $ M.fromList $ zip zs2 (zip zs (repeat (-1))) :: IO Jmap

p83 :: Jmap -> Jmap
p83 m = g m2 msize2  where
    (cost,_) = m ! (1,1)
    m' = M.insert (1,1) (cost,cost) m
    msize2 = 2
    m2 = f m' [(1,1,cost)] msize2

--g :: Jmap -> Int -> Jmap
g m msize2 = if msize2' < msize  then g m' msize2'  else m'  where
    vl = map (\((i,j),(c,tc)) -> (i,j,tc))
	$ M.toList $ M.filterWithKey (\(i,j) v->j==msize2) m
    msize2' = msize2+1
    m' = f m vl msize2'

f m [] msize2 = m
f m ((i,j,tcost):ns) msize2 = f m4 ns4 msize2  where
    (ns1,m1) = check (i,j+1) m tcost msize2 ns
    (ns2,m2) = check (i,j-1) m1 tcost msize2 ns1
    (ns3,m3) = check (i+1,j) m2 tcost msize2 ns2
    (ns4,m4) = check (i-1,j) m3 tcost msize2 ns3

check !(!i,!j) !m !tcost !msize2 !ns
    | i<1 || i>msize || j<1 || j>msize2 = (ns,m)
    | otherwise  = check2 (i,j) m tcost ns

check2 !(!i,!j) !m !itcost !ns = ans where
    (!cost,!tcost) = m ! (i,j)
    ncost = itcost + cost
    !ans = if tcost < 0 || ncost < tcost
	then ( (i,j,ncost):ns,  M.insert (i,j) (cost,ncost) m)
        else (ns,m)

main = do
    zm <- getmap
    print $ (p83 zm) ! (80,80)
