module Problems.Problem3 (problem3p1, problem3p2) where

import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Utils (convertIO, getFile)

problem3p1 :: Int
problem3p1 = sum $ f line
  where
    line = convertIO $ getFile "src/Problems/Input/Problem3.txt"
    f :: String -> [Int]
    f [] = []
    f (_ : xs)
      | "mul(" `isPrefixOf` xs = g xs 8 : f xs
      | otherwise = f xs
    g :: String -> Int -> Int
    g _ 13 = 0
    g str i =
      case readMaybe (drop 3 $ take i str) :: Maybe (Int, Int) of
        Just (a, b) -> a * b
        Nothing -> g str (i + 1)

problem3p2 :: Int
problem3p2 = sum $ f line True
  where
    line = convertIO $ getFile "src/Problems/Input/Problem3.txt"
    f :: String -> Bool -> [Int]
    f [] _ = []
    f (_ : xs) dont
      | dont && "mul(" `isPrefixOf` xs = g xs 8 : f xs dont
      | "don't()" `isPrefixOf` xs = f xs False
      | "do()" `isPrefixOf` xs = f xs True
      | otherwise = f xs dont
    g :: String -> Int -> Int
    g _ 13 = 0
    g str i =
      case readMaybe (drop 3 $ take i str) :: Maybe (Int, Int) of
        Just (a, b) -> a * b
        Nothing -> g str (i + 1)
