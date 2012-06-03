-- Triangle Numbers - 1 3 6 10 15 - nth element = sum [1..n]
tris = map (\x-> quot ((x+1)*x) 2) [1..]
