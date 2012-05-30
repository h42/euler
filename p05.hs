good m n = all (\x->rem n x == 0) [2..m]

ps = product [2,3,5,7,11,13,17,19]
f m = head $ filter (good m) [ps, (2*ps) ..]

main = print $ f 20





