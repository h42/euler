import Primes
import Control.Monad

p999 = takeWhile (<1000) primes

coefs :: [(Int,Int)]
coefs = [(a,b) | b<-p999, a<-[-999,-997..999] ]

checkquads (a,b) = checkquad2 a b 0
checkquad2 a b n =
    let y = n*n + a*n + b
    in  if y>1 && isPrime primes y then checkquad2 a b (n+1) else (n,a,b)

main = do
    --tester (-999) 61 10

    let ans = map checkquads coefs
	(n,a,b) = maximum ans
    print $ length ans
    print $ take 5 ans
    print $ (n,a,b)
    print $ a*b

