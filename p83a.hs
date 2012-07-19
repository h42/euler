import Data.Array.ST
import Data.Array
import Control.Monad

type Jar = Array (Int,Int) (Int,Int)
msize = 80

getmap :: IO [Int]
getmap = do
    s <- readFile "p82.txt"
    return $ map read $ words $ map (\x->if x==',' then ' ' else x) s

p83 :: [Int] -> Jar
p83 ns = runSTArray $ do
    arr <- newListArray ((1,1),(80,80)) (zip ns (repeat (-1)))
    (cost,_) <- readArray arr (1,1)
    writeArray arr (1,1) (cost,cost)
    let msize2 = 2
    f arr [(1,1,cost)] msize2
    g arr msize2
    return arr

g arr msize2 = do
    let msize2' = msize2+1
    vl <- forM [1..80] $ \i -> do
	(cost,tcost) <- readArray arr (i,msize2)
	return (i,msize2,tcost)
    f arr vl msize2'
    if msize2' < msize  then g arr msize2'  else return ()

f arr [] msize2 = return ()
f arr ((i,j,tcost):ns) msize2 = do
    ns1 <- check (i,j+1) arr tcost msize2 ns
    ns2 <- check (i,j-1) arr tcost msize2 ns1
    ns3 <- check (i+1,j) arr tcost msize2 ns2
    ns4 <- check (i-1,j) arr tcost msize2 ns3
    f arr ns4 msize2

check (i,j) arr tcost msize2 ns
    | i<1 || i>msize || j<1 || j>msize2 = return ns
    | otherwise  = check2 (i,j) arr tcost ns

check2 (i,j) arr itcost ns = do
    (cost,tcost) <- readArray arr (i,j)
    let ncost = itcost + cost
    if tcost < 0 || ncost < tcost then do
	writeArray arr (i,j) (cost,ncost)
	return ((i,j,ncost):ns)
    else return ns

main = do
    zm <- getmap
    let arr = p83 zm
    print $ arr ! (80,80)
