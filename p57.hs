p57 0 _ _ ans = ans
p57 i x y ans = p57 (i-1) x2 y2 ans' where
    y2 = y*2+x
    x2 = y
    ans' = if length (show (x2+y2)) > length (show y2) then ans+1 else ans

main = print $ p57 999 1 2 0
