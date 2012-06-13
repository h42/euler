import Data.Ratio

p33 (a,b) = ans where
    (qa,ra) = quotRem a 10
    (qb,rb) = quotRem b 10
    ans =  rb /= 0  &&  ra == qb  &&  qa % rb == a % b

main = do
    let curious = map (\(a,b)->a%b)
		      (filter p33 [(a,b) | b<-[11..99], a<-[10..98], a<b])
    print curious
    print $ product curious

