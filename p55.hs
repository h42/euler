isLychrel 50 _  = True
isLychrel i x = if p == reverse p then False else isLychrel (i+1) (x+y)  where
    y = read $ reverse $ show x
    p = show (x+y)

main = print $ length $ filter (isLychrel 0) [10..9999]
