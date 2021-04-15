-- Copyright (c) 2015-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
import Test (getMaxTestsFromArgs)

import System.Exit (exitFailure)
import Data.List (elemIndices,sort)

import Test.LeanCheck.Error
import Test.LeanCheck.Utils.Types (Nat)

main :: IO ()
main  =  do
  max <- getMaxTestsFromArgs 200
  case elemIndices False (tests max) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests :: Int -> [Bool]
tests n  =
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

  , errorToNothing ()                   ==  Just ()
  , errorToNothing (undefined   :: ())  ==  Nothing
  , errorToNothing (error "err" :: ())  ==  Nothing

  , errorToNothing False                  ==  Just False
  , errorToNothing True                   ==  Just True
  , errorToNothing (undefined   :: Bool)  ==  Nothing
  , errorToNothing (error "err" :: Bool)  ==  Nothing

  , errorToNothing (0           :: Int)  ==  Just 0
  , errorToNothing (1           :: Int)  ==  Just 1
  , errorToNothing (undefined   :: Int)  ==  Nothing
  , errorToNothing (error "err" :: Int)  ==  Nothing
  , errorToNothing (1 `div` 0   :: Int)  ==  Nothing

  , errorToNothing (head "abc")  ==  Just 'a'
  , errorToNothing (tail "abc")  ==  Just "bc"
  , errorToNothing (head "")  ==  Nothing
  , errorToNothing (tail "")  ==  Nothing

  , errorToLeft ()                   ==  Right ()
  , errorToLeft (undefined   :: ())  ==  Left "Prelude.undefined"
  , errorToLeft (error "err" :: ())  ==  Left "err"

  , errorToLeft False                  ==  Right False
  , errorToLeft True                   ==  Right True
  , errorToLeft (undefined   :: Bool)  ==  Left "Prelude.undefined"
  , errorToLeft (error "err" :: Bool)  ==  Left "err"

  , errorToLeft (0           :: Int)  ==  Right 0
  , errorToLeft (1           :: Int)  ==  Right 1
  , errorToLeft (undefined   :: Int)  ==  Left "Prelude.undefined"
  , errorToLeft (error "err" :: Int)  ==  Left "err"
  , errorToLeft (1 `div` 0   :: Int)  ==  Left "divide by zero"

  , errorToLeft (head "abc")  ==  Right 'a'
  , errorToLeft (tail "abc")  ==  Right "bc"

  , errorToLeft (head "")  ==  Left "Prelude.head: empty list" ||    -- GHC
    errorToLeft (head "")  ==  Left "pattern match failure: head []" -- Hugs

  , errorToLeft (tail "")  ==  Left "Prelude.tail: empty list" ||    -- GHC
    errorToLeft (tail "")  ==  Left "pattern match failure: tail []" -- Hugs
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
