import System.Random
import Control.Monad
import Data.List
import qualified Data.IntMap as M

type Board = M.IntMap Int

play :: [Int] -> Board -> Int -> Board
play [] board pos = score board
play (d:dice) board pos = play dice board' pos' where
    pos' = rem (pos + d) 40
    board' = M.insertWith (+) pos' 1 board

score board = board

rollDice :: Int -> IO [Int]
rollDice n = do
    g <- getStdGen
    let n2 = quot n 2
	rs' = take n $ randomRs (1,6) g :: [Int]
    return $ zipWith (+) (take n2 rs') (drop n2 rs')

main = do
    --let dice = concat $ repeat [x+y | x<-[1..6], y<-[6,5..1]]
    let board = M.fromList [(i,0) | i<-[0..39]]
    dice <- rollDice 18000
    print $ take 5 dice
    let b = play dice board 1
    print $ sortBy (\x y->compare (snd x) (snd y))
	      $ filter (\(k,v)-> v>0) $ M.toList b
