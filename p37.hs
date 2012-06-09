import Primes

p37 x
    | x < 10 = False
    | otherwise = p37a x && p37b (num2list x)

p37a x
    | x<10 = True
    | otherwise = if isPrime primes x2  then p37a x2  else False
  where x2 = quot x 10

p37b [x] = elem x [2,3,5,7]
p37b (n:nx) = if isPrime primes (list2num nx)  then p37b nx  else False

main = do
    let ps = take 11 $ filter p37 primes
    print $ ps
    print $ sum ps
