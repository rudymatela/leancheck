-- Copyright (c) 2015-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import System.Exit (exitFailure)
import Data.List (elemIndices)
import Test.LeanCheck.Function ()

main :: IO ()
main =
  case elemIndices False (tests 200) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests :: Int -> [Bool]
tests n =
  [ True
  , fails n prop_foldlr
  , holds n prop_foldlr'
  , fails n prop_mapFilter
  , fails n prop_false
  , fails n prop_false'
  ]

type A = Int

prop_foldlr :: (A -> A -> A) -> A -> [A] -> Bool
prop_foldlr f z xs  =  foldr f z xs == foldl f z xs

-- correct version of the property above
prop_foldlr' :: (A -> A -> A) -> A -> [A] -> Bool
prop_foldlr' f z xs  =  foldl (flip f) z (reverse xs) == foldr f z xs

prop_mapFilter :: (A -> A) -> (A -> Bool) -> [A] -> Bool
prop_mapFilter f p xs  =  filter p (map f xs) == map f (filter p xs)

prop_false :: (A -> A) -> (A -> A) -> Bool
prop_false _ _ = False

prop_false' :: (A -> A) -> (A,A) -> Bool
prop_false' _ _ = False
