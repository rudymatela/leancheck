-- Copyright (c) 2015-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
import Test ()

import System.Exit (exitFailure)
import Data.List (elemIndices,sort)

import Test.LeanCheck.Error
import Test.LeanCheck.Utils.Types (Nat)

main :: IO ()
main =
  case elemIndices False (tests 200) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests :: Int -> [Bool]
tests n =
  [ True

  , not $ holds     n prop_sortMinE
  , fails           n prop_sortMinE
  , counterExample  n prop_sortMinE == Just ["[]"]
  , counterExamples n prop_sortMinE == [["[]"]]

  , holds           n prop_sortMin
  , not $ fails     n prop_sortMin
  , counterExample  n prop_sortMin == Nothing
  , counterExamples n prop_sortMin == []

  , exists          n someNumbers
  , witness         n someNumbers == Just ["2"]
  , witnesses       n someNumbers == map ((:[]).show) [2,3,5,7,11,13,17]

  , exists          n someOthers
  , witness         n someOthers == Just ["2","2"]
  , witnesses  (20*n) someOthers == (map.map) show [[2,2],[2,4],[3,3]
                                                   ,[2,8],[3,9],[3,27]]
  ]

prop_sortMinE :: [Nat] -> Bool
prop_sortMinE xs = head (sort xs) == minimum (xs::[Nat])

prop_sortMin :: [Nat] -> Bool
prop_sortMin xs = not (null xs)
              ==> head (sort xs) == minimum (xs::[Nat])

someNumbers :: Int -> Bool
someNumbers  2 = True
someNumbers  3 = True
someNumbers  5 = True
someNumbers  7 = True
someNumbers 11 = True
someNumbers 13 = True
someNumbers 17 = True

someOthers :: Int -> Int -> Bool
someOthers = \x -> case x of 2 -> \y -> case y of 2 -> True
                                                  4 -> True
                                                  8 -> True
                             3 -> \y -> case y of 3 -> True
                                                  9 -> True
                                                  27 -> True
