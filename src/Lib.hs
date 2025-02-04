module Lib
  ( solve,
  )
where

import Solver (solver)

solve :: IO ()
solve = do
  print (solver 4 1)
