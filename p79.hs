import Data.List

-- Simple to work out by hand - will I regret this ?

main = do
    fs <- readFile "p79.txt"
    let ds = map (take 3) $ lines fs
    print $ nub $ concat ds
    mapM print $ nub $ sort ds
