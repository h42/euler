import qualified Data.Sequence as S
import Data.Sequence (Seq,fromList,(<|),(|>))

s = fromList [1..7]

main = do
    let s1 = (22 <| s) |> (-4)
    print s1
    print $ S.index s1 0
    print $ S.drop 1 s1
