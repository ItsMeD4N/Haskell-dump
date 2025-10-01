module SecondLargest where

secondLargest :: [Int] -> Int
secondLargest l = maxList (removeX (maxList l) l)

maxList :: [Int] -> Int
maxList l =
  if isOneElmt l then last l
  else max2 (last l) (maxList (init l))

removeX :: Int -> [Int] -> [Int]
removeX x l =
  if isEmpty l then []
  else if head l == x then tail l
       else konso (head l) (removeX x (tail l))

max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b

konso :: Int -> [Int] -> [Int]
konso e l = [e] ++ l

isEmpty :: [Int] -> Bool
isEmpty l = null l

isOneElmt :: [Int] -> Bool
isOneElmt l = (length l) == 1
