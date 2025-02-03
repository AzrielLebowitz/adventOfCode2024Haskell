module Problems.Problem1 (problem1p1, problem1p2) where

import Data.List (group, sort)
import Utils (convertIO, getFile)

problem1p1 :: Int
problem1p1 = sum $ map abs $ zipWith (-) arr2 arr1
  where
    file = convertIO $ getFile "src/Problems/Input/Problem1.txt"
    fileWords = map words $ lines file
    arr1 = sort $ map (read . head) fileWords
    arr2 = sort $ map (read . last) fileWords

problem1p2 :: Int
problem1p2 = sum $ zipWith (*) arr1 $ map countOnLeft arr1
  where
    file = convertIO $ getFile "src/Problems/Input/Problem1.txt"
    fileWords = map words $ lines file
    arr1 = map (read . head) fileWords
    arr2 = [(last x, length x) | x <- group $ sort $ map (read . last) fileWords]
    countOnLeft num = last $ 0 : (map snd $ filter (\(x, _) -> x == num) arr2)
