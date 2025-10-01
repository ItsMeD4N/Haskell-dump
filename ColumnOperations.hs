module ColumnOperations where 

konsdot :: [Int] -> Int -> [Int]
konso :: Int -> [Int] -> [Int]
isEmpty :: [Int] -> Bool

-- REALISASI
konso e l = [e] ++ l
konsdot l e = l ++ [e]
isEmpty l = null l

-- Lengkapi Fungsi di bawah ini
transposeMatrix :: [[Int]] -> [[Int]]
transposeMatrix [] = []
transposeMatrix ([]:_) = []
transposeMatrix m = (map head m) : transposeMatrix (map tail m)

-- Fungsi utama
columnOperations :: [[Int]] -> [Int]
columnOperations matrix
    | null matrix || null (head matrix) = []
    | otherwise =
        let
            columns = transposeMatrix matrix
            operations = cycle [maximum, minimum, sum]
        in
            zipWith ($) operations columns