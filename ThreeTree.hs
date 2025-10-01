module ThreeTree where

threeTree :: [Int] -> [Int]
threeTree a = if (isEmpty a) then []
                else if (head a `mod` 3 == 0) then
                    konso (head a) (threeTree (tail a))
                else
                    threeTree (tail a)

konso :: Int -> [Int] -> [Int]
konso e l = [e] ++ l

isEmpty :: [Int] -> Bool
isEmpty l = null l