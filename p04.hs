
checker x = ans2 where
    (q1,r1) = quotRem x 10
    (q2,r2) = quotRem q1 10
    (q3,r3) = quotRem q2 10
    (q4,r4) = quotRem q3 10
    (q5,r5) = quotRem q4 10
    (q6,r6) = quotRem q5 10
    ans2 = if r1 == r6 && r2 == r5 && r3 == r4 then True else False

ul = 999
xy :: [(Int,Int)]
xy = do
    x <- [100..ul]
    y <- [100..ul]
    return (x,y)

f1 (ax,ay) (vx,vy) = ans  where
    an = ax * ay
    vn = vx * vy
    ans = if vn > an && checker vn then (vx,vy) else (ax,ay)

main = do
    let (x,y) = foldl f1 (0,0) xy
    print (x*y)
    print (x,y)

