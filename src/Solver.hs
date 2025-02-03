module Solver (solver) where

import Problems.Problem1 (problem1)

solver :: Int -> Int
solver n
  | n == 1 = problem1
  | otherwise = 0
