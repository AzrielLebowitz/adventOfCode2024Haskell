module Solver (solver) where

import Problems.Problem1 (problem1p1, problem1p2)
import Problems.Problem2 (problem2p1, problem2p2)
import Problems.Problem3 (problem3p1, problem3p2)
import Problems.Problem4 (problem4p1, problem4p2)

solver :: Int -> Int -> Int
solver n p
  | n == 1 && p == 1 = problem1p1
  | n == 1 && p == 2 = problem1p2
  | n == 2 && p == 1 = problem2p1
  | n == 2 && p == 2 = problem2p2
  | n == 3 && p == 1 = problem3p1
  | n == 3 && p == 2 = problem3p2
  | n == 4 && p == 1 = problem4p1
  | n == 4 && p == 2 = problem4p2
  | otherwise = 0
