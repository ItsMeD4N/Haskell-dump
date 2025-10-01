module RunLengthEncoding where

runLengthEncoding :: String -> [(Char, Int)]
runLengthEncoding l =
  if isEmpty l then []
  else konso (encode (head l) l) (runLengthEncoding (removeSame (head l) l))

encode :: Char -> String -> (Char, Int)
encode x l = (x, countSame x l)

countSame :: Char -> String -> Int
countSame x l =
  if isEmpty l then 0
  else if head l == x then 1 + countSame x (tail l)
       else 0

removeSame :: Char -> String -> String
removeSame x l =
  if isEmpty l then []
  else if head l == x then removeSame x (tail l)
       else l

konso :: a -> [a] -> [a]
konso e l = [e] ++ l

isEmpty :: [a] -> Bool
isEmpty l = null l
