-- Copyright (c) 2015-2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import System.Exit (exitFailure)
import Data.List (elemIndices, sort, nub, delete)

import Test.LeanCheck
import Test.LeanCheck.Invariants
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

  , holds 100 $ ptofApp -:> (char,char)
  , holds 100 $ associationsValues int  100 -:> [int]
  , holds 100 $ associationsValues bool 100 -:> [bool]
  , holds 500 $ associationsNewAndOld -: [int]  >- [[int]]  >- und
  , holds 500 $ associationsNewAndOld -: [int]  >- [[bool]] >- und
  , holds 500 $ associationsNewAndOld -: [bool] >- [[bool]] >- und
  , holds 500 $ associationsNewAndOld -: [bool] >- [[int]]  >- und
  ]

ptofApp :: (Ord a, Eq b) => (a,b) -> [(a,b)] -> Bool -- Ord a is just for allUnique
ptofApp (x,y) ps = (x,y) `elem` ps && allUnique (map fst ps)
               ==> pairsToFunction ps x == y

associationsValues :: (Listable b, Eq a)
                   => b -> Int -> [a] -> Bool
associationsValues ty n xs = all (\xs' -> map fst xs' == xs)
                           $ take n
                           $ concat
                           $ associations xs (tiers `asTypeOf` [[ty]])

associationsNewAndOld :: (Eq a, Eq b) => [a] -> [[b]] -> Bool
associationsNewAndOld xs yss = associations xs yss == oldAssociations xs yss
  where oldAssociations xs sbs = mapT (zip xs) (listsOfLength (length xs) sbs)

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique (filter (< x) xs)
                && allUnique (filter (> x) xs)
