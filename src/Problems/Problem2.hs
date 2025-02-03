module Problems.Problem2 (problem2p1, problem2p2) where

import Data.List (group, nub, sort)
import Utils (convertIO, getFile)

problem2p1 :: Int
problem2p1 = length $ filter (\x -> isSafeMinus x || isSafePlus x) arr
  where
    file = convertIO $ getFile "src/Problems/Input/Problem2.txt"
    fileWords = map words $ lines file
    differences xs = zipWith (-) xs (tail xs)
    arr = map (map read) fileWords
    isSafeMinus xs = all (\x -> -1 >= x && x >= -3) $ differences xs
    isSafePlus xs = all (\x -> 1 <= x && x <= 3) $ differences xs

problem2p2 :: Int
problem2p2 = length $ filter (/= 0) (map (length . filter (\y -> isSafePlus y || isSafeMinus y) . removeEach) arr)
  where
    file = convertIO $ getFile "src/Problems/Input/Problem2.txt"
    fileWords = map words $ lines file
    differences xs = zipWith (-) xs (tail xs)
    arr = map (map read) fileWords
    removeEach xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]
    isSafeMinus xs = all (\x -> -1 >= x && x >= -3) $ differences xs
    isSafePlus xs = all (\x -> 1 <= x && x <= 3) $ differences xs
