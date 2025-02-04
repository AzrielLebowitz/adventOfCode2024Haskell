module Problems.Problem4 (problem4p1) where

import Utils (convertIO, getFile)

problem4p1 :: Int
problem4p1 = length $ filter (\x -> x == "XMAS" || x == "SAMX") $ [[f i j 0, f i j 1, f i j 2, f i j 3] | i <- [0 .. (len - 4)], j <- [0 .. (len - 4)], f <- [diagonalLR, diagonalRL]] ++ [[f i j 0, f i j 1, f i j 2, f i j 3] | i <- [0 .. (len - 4)], j <- [0 .. (len - 1)], f <- [vertical, horizontal]]
  where
    grid :: [String]
    grid = lines . convertIO $ getFile "src/Problems/Input/Problem4.txt"
    len = length $ grid !! 1
    vertical i j n = grid !! j !! (i + n)
    horizontal i j n = grid !! (i + n) !! j
    diagonalLR i j n = grid !! (i + n) !! (j + n)
    diagonalRL i j n = grid !! (len - 1 - i - n) !! (j + n)
