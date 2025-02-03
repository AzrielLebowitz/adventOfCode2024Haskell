module Problems.Problem2 (problem2p1) where

import Data.List (group, nub, sort)
import Utils (convertIO, getFile)

problem2p1 :: Int
problem2p1 = length $ filter (\x -> isSafeMinus x || isSafePlus x) arr1
  where
    file = convertIO $ getFile "src/Problems/Input/Problem2.txt"
    fileWords = map words $ lines file
    differences xs = zipWith (-) xs (tail xs)
    arr1 = map (map read) fileWords
    isSafeMinus xs = all (\x -> -1 >= x && x >= -3) $ differences xs
    isSafePlus xs = all (\x -> 1 <= x && x <= 3) $ differences xs
