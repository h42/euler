import Euler

main = do
    putStrLn $ show 117 ++ " - " ++ show (ilen 117)
    putStrLn $ show 1234567 ++ " - " ++ show (ilen 1234567)
    print $ digitN 1234567 4
    mapM print (map (digitN 1234567) [0..7])

