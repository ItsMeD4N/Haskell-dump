module Matrix where 

-- UTILITY FUNCTIONS
konsdot :: [Int] -> Int -> [Int]
konso :: Int -> [Int] -> [Int]
isEmpty :: [Int] -> Bool

konso e l = [e] ++ l
konsdot l e = l ++ [e]
isEmpty l = null l

konsdotMatrix :: [[Int]] -> [Int] -> [[Int]]
konsoMatrix :: [Int] -> [[Int]] -> [[Int]]
isEmptyMatrix :: [[Int]] -> Bool

konsoMatrix e l = [e] ++ l
konsdotMatrix l e = l ++ [e]
isEmptyMatrix l = null l

-- NOTES: Semua Index dipastikan valid, yaitu tidak negatif dan tidak melebihi panjang list/Matrix

-- MATRIX MANIPULATION FUNCTIONS
-- TYPE: Matrix adalah [[Int]]
-- Dengan definisi type sebagai berikut, kita dapat mensubstitusi [[Int]] dengan Matrix
type Matrix = [[Int]]

-- addAtIndex :: Int -> [Int] -> Int -> [Int]
-- {addAtIndex x l i menambahkan elemen x pada posisi i dalam l}
-- 
-- Note: list l mungkin kosong
--       Mengembalikan [x] apabila l kosong
addAtIndex :: Int -> [Int] -> Int -> [Int]
addAtIndex x l i = (take i l) ++ [x] ++ (drop i l)

-- deleteAtIndex :: [Int] -> Int -> [Int]
-- {deleteAtIndex l i menghapus elemen pada posisi i dalam l}
-- 
-- Note: list l mungkin kosong
--       Mengembalikan [] apabila l kosong
deleteAtIndex :: [Int] -> Int -> [Int]
deleteAtIndex l i = (take i l) ++ (drop (i + 1) l)

-- editAtIndex :: [Int] -> Int -> Int -> [Int]
-- {editAtIndex l i newValue mengubah elemen l pada posisi i dengan newValue}
-- 
-- Note: list l mungkin kosong
--       Mengembalikan [] apabila l kosong
editAtIndex :: [Int] -> Int -> Int -> [Int]
editAtIndex [] _ _ = []
editAtIndex l i newValue = (take i l) ++ [newValue] ++ (drop (i + 1) l)

-- addMatrixElement :: Matrix -> Int -> Int -> Int -> Matrix
-- {addMatrixElement m row col value menambahkan value pada posisi (row,col) dalam m}
-- 
-- Note: Matrix m mungkin kosong
--       Mengembalikan [] apabila m kosong
addMatrixElement :: Matrix -> Int -> Int -> Int -> Matrix
addMatrixElement [] _ _ _ = []
addMatrixElement m row col value = 
    let
        targetRow = m !! row
        newRow = addAtIndex value targetRow col
    in
        (take row m) ++ [newRow] ++ (drop (row + 1) m)

-- deleteMatrixElement :: Matrix -> Int -> Int -> Matrix  
-- {deleteMatrixElement m row col menghapus elemen pada posisi (row,col) dalam m}
-- 
-- Note: Matrix m mungkin kosong
--       Mengembalikan [] apabila m kosong
deleteMatrixElement :: Matrix -> Int -> Int -> Matrix
deleteMatrixElement [] _ _ = []
deleteMatrixElement m row col =
    let
        targetRow = m !! row
        newRow = deleteAtIndex targetRow col
    in
        (take row m) ++ [newRow] ++ (drop (row + 1) m)

-- editMatrixElement :: Matrix -> Int -> Int -> Int -> Matrix
-- {editMatrixElement m row col newValue mengubah elemen pada posisi (row,col) dengan newValue}
-- 
-- Note: Matrix m mungkin kosong
--       Mengembalikan [] apabila m kosong
editMatrixElement :: Matrix -> Int -> Int -> Int -> Matrix
editMatrixElement [] _ _ _ = []
editMatrixElement m row col newValue =
    let
        targetRow = m !! row
        newRow = editAtIndex targetRow col newValue
    in
        (take row m) ++ [newRow] ++ (drop (row + 1) m)

-- getMatrixElement :: Matrix -> Int -> Int -> Int
-- {getMatrixElement m row col mengambil elemen pada posisi (row,col) dalam m}
-- 
-- Note: Matrix m mungkin kosong
--       Mengembalikan 0 apabila m kosong
getMatrixElement :: Matrix -> Int -> Int -> Int
getMatrixElement [] _ _ = 0
getMatrixElement m row col = (m !! row) !! col


-- printMatrix :: Matrix -> IO ()
-- {printMatrix m mencetak m dalam format yang mudah dibaca}
printMatrix :: Matrix -> IO ()
printMatrix [] = putStrLn "Empty m"
printMatrix m = mapM_ print m