-- File: PohonBinerDetail.hs
-- Modul ini berisi implementasi lengkap struktur data Pohon Biner (BinTree)
-- beserta semua fungsi selektor, predikat, dan lainnya sesuai dengan
-- materi IF2110 yang diberikan.

module PohonBinerDetail where

-- ## Tipe Data dan Konstruktor ## --
[cite_start]-- [cite: 367]
-- Deklarasi tipe data pohon biner, bisa berisi elemen tipe apa saja (generik 'a').
-- Sebuah pohon bisa 'Empty' (kosong) atau berupa 'Node' yang memiliki nilai,
-- sub-pohon kiri, dan sub-pohon kanan.
data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show, Eq)

[cite_start]-- [cite: 369]
-- Fungsi pembantu untuk membuat sebuah Node baru dalam pohon.
makeBinTree :: a -> BinTree a -> BinTree a -> BinTree a
makeBinTree a l r = Node a l r

-- ## Selektor ## --
[cite_start]-- [cite: 390, 391, 392, 393, 394, 395]
-- Fungsi untuk mendapatkan komponen dari sebuah pohon.

-- akar p adalah akar dari p. Jika p adalah //L A R\\ maka akar p = A
akar :: BinTree a -> a
akar (Node a l r) = a

-- left p adalah sub pohon kiri dari p. Jika p adalah //L A R\\ maka left p = L
left :: BinTree a -> BinTree a
left (Node a l r) = l

-- right p adalah sub pohon kanan dari p. Jika p adalah //L A R\\ maka right p = R
right :: BinTree a -> BinTree a
right (Node a l r) = r

-- ## Predikat ## --
[cite_start]-- [cite: 401, 402, 403, 404, 405, 406, 407, 408]
-- Fungsi yang mengembalikan nilai Boolean untuk memeriksa properti pohon.

-- isTreeEmpty p true jika p adalah // \\
isTreeEmpty :: BinTree a -> Bool
isTreeEmpty Empty = True
isTreeEmpty (Node a l r) = False

-- isOneElmt p true jika p adalah //A\\
isOneElmt :: BinTree a -> Bool
isOneElmt p =
    not (isTreeEmpty p) &&
    isTreeEmpty (left p) && isTreeEmpty (right p)

[cite_start]-- [cite: 413, 414, 415, 416, 417, 418, 419]
-- isUnerLeft p true jika p hanya mengandung sub pohon kiri, p adalah //L A\\
isUnerLeft :: BinTree a -> Bool
isUnerLeft p = not (isTreeEmpty p) && not (isTreeEmpty (left p)) && isTreeEmpty (right p)

-- isUnerRight p true jika p hanya mengandung sub pohon kanan, p adalah //A R\\
isUnerRight :: BinTree a -> Bool
isUnerRight p = not (isTreeEmpty p) && isTreeEmpty (left p) && not (isTreeEmpty (right p))

-- isBiner p true jika p mengandung sub pohon kiri dan sub pohon kanan, p adalah // L A R \\
isBiner :: BinTree a -> Bool
isBiner p = not (isTreeEmpty p) && not (isTreeEmpty (left p)) && not (isTreeEmpty (right p))

[cite_start]-- [cite: 425, 426, 427, 428, 429, 430]
-- isExistLeft p true jika p mengandung sub pohon kiri
isExistLeft :: BinTree a -> Bool
isExistLeft p =
    not (isTreeEmpty p) && not (isTreeEmpty (left p))

-- isExistRight p true jika p mengandung sub pohon kanan
isExistRight :: BinTree a -> Bool
isExistRight p =
    not (isTreeEmpty p) && not (isTreeEmpty (right p))

-- ## Fungsi Rekursif Lainnya ## --

[cite_start]-- [cite: 437, 438, 439, 440, 441, 442, 443, 444, 445, 446]
-- NbElmt p memberikan banyaknya elemen dari pohon p
nbElmt :: BinTree a -> Int
nbElmt p =
    if isTreeEmpty p then -- basis
        0
    else -- rekurens
        nbElmt (left p) + 1 + nbElmt (right p)

[cite_start]-- [cite: 452, 453, 454, 455, 456, 457, 458, 459, 460, 461, 462, 463]
-- nbDaun p memberikan banyaknya daun dari pohon p
nbDaun :: BinTree a -> Int
nbDaun p =
    if isTreeEmpty p then 0 else nbDaun1 p

-- nbDaun1 p memberikan banyaknya daun dari pohon p yang tidak kosong
nbDaun1 :: BinTree a -> Int
nbDaun1 p =
    if isOneElmt p then 1 -- basis
    else -- rekurens
        if isBiner p then nbDaun1 (left p) + nbDaun1 (right p)
        else if isUnerLeft p then nbDaun1 (left p)
        else nbDaun1 (right p) -- kasus isUnerRight

-- ## Contoh Pohon ## --
[cite_start]-- [cite: 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384]
-- Konstruksi sebuah BinTree dengan elemen Integer sesuai contoh di PDF.
pohonContoh :: BinTree Int
pohonContoh =
  makeBinTree 3
    (makeBinTree 15
      Empty
      (makeBinTree 25 Empty Empty))
    (makeBinTree 10
      (makeBinTree 50 Empty Empty)
      (makeBinTree 30 Empty Empty))