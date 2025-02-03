module Problems.Problem1 (problem1) where

import Data.List (sort)
import Utils (convertIO, getFile)

problem1 :: Int
problem1 = sum $ map abs $ zipWith (-) arr2 arr1
  where
    file = convertIO $ getFile "src/Problems/Input/Problem1.txt"
    fileWords = map words $ lines file
    arr1 = sort $ map (read . head) fileWords
    arr2 = sort $ map (read . last) fileWords
