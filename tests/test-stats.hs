-- Copyright (c) 2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import System.Exit (exitFailure)
import Data.List (elemIndices,sort)
import Test.LeanCheck
import Test.LeanCheck.Stats

import Data.List (isPrefixOf)
import Data.Function (on)

main :: IO ()
main = do
  classStats 100 (odd     :: Int -> Bool)
  classStats 100 (even    :: Int -> Bool)
  classStats 100 (ordered :: [Int] -> Bool)
  classStats 100 (sum     :: [Int] -> Int)
  classStats 100 (length  :: [Int] -> Int)
  classStats 100 (take 1  :: [Int] -> [Int])
  conditionStats 1000 [odd :: Int -> Bool,even]
  conditionStats 1000 [ordered :: [Int] -> Bool, ordered . reverse]
  case elemIndices False (tests 100) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests :: Int -> [Bool]
tests n =
  [ True

  , classify                   [1,2,3,4,1,2,3,1,2,1] == [[1,1,1,1],[2,2,2],[3,3],[4]]
  , classifyBy ((==) `on` odd) [1,2,3,4,1,2,3,1,2,1] == [[1,3,1,3,1,1],[2,4,2,2]]
  , classifyOn odd             [1,2,3,4,1,2,3,1,2,1] == [[1,3,1,3,1,1],[2,4,2,2]]
  , counts                   [1,2,3,4,1,2,3,1,2,1] == [(1,4),(2,3),(3,2),(4,1)]
  , countsBy ((==) `on` odd) [1,2,3,4,1,2,3,1,2,1] == [(1,6),(2,4)]
  , countsOn odd             [1,2,3,4,1,2,3,1,2,1::Int] == [(True,6),(False,4)]
  ]
