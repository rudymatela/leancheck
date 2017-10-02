-- Copyright (c) 2015-2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import System.Exit (exitFailure)
import Data.List (elemIndices, sort, nub, delete)

import Test.LeanCheck
import Test.LeanCheck.Function.ListsOfPairs
import Test.LeanCheck.Utils


main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests =
  [ True
  -- TODO: add some tests here
  ]

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique (filter (< x) xs)
                && allUnique (filter (> x) xs)
