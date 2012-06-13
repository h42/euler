
--(d+=m<3?y--:y-2,23*m/9+d+4+y/4-y/100+y/400)%7

weekDay :: Int -> Int -> Int -> Int
weekDay y m d = dow' where
    (d2,y2) = if m < 3 then (d + y, y - 1) else (d + y - 2,y)
    dow' = (23 * m `quot` 9 + d2 + 4 + y2 `quot` 4 -
	       y2 `quot` 100 + y2 `quot` 400) `mod` 7

sun20 = length $ filter (==0) (map f days) where
    days = [(y,m) | y<-[1901..2000], m<-[1..12]]
    f (y,m) = weekDay y m 1

main = print sun20
