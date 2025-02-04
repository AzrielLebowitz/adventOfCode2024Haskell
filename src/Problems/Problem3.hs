module Problems.Problem3 (problem3p1) where

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
    g str 13 = 0
    g str i =
      case readMaybe (drop 3 $ take i str) :: Maybe (Int, Int) of
        Just (a, b) -> a * b
        Nothing -> g str (i + 1)
