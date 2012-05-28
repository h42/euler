
main = print $ sum [x | x <- [3..999] ,rem x 3 == 0 || rem x 5 == 0]

