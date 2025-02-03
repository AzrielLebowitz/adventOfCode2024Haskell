module Solver (solver) where

import Problems.Problem1 (problem1p1, problem1p2)

solver :: Int -> Int -> Int
solver n p
  | n == 1 && p == 1 = problem1p1
  | n == 1 && p == 2 = problem1p2
  | otherwise = 0
