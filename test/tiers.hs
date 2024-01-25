-- Copyright (c) 2015-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

-- import Test.LeanCheck -- already exported by Test
import Test.LeanCheck.Utils
import Test.LeanCheck.Tiers
import Data.List (sort, nub, delete)

main :: IO ()
main  =  mainTest tests 200

tests :: Int -> [Bool]
tests n =
  [ True

  , checkNoDup 12
  , checkBags 18
  , checkSets 20
  , checkDistinctPairs 20
  , checkUnorderedPairs 20
  , checkUnorderedDistinctPairs 20
  , checkLengthListingsOfLength 5 5
  , checkSizesListingsOfLength 5 5

  , all (uncurry (/=)) . concat . take 200 $ distinctPairs (tiers :: [[Nat]])

  , productMaybeWith ($) [[const Nothing, Just]] [[1],[2],[3],[4]]
    == [[1],[2],[3],[4]]
  , productMaybeWith (flip ($))
                     [[1],[2],[3],[4]]
                     [[const Nothing],[Just]] == [[],[1],[2],[3],[4]]

  , holds n $ deleteTIsMapDelete 10 -:> nat
  , holds n $ deleteTIsMapDelete 10 -:> int
  , holds n $ deleteTIsMapDelete 10 -:> bool
  , holds n $ deleteTIsMapDelete 10 -:> int2

  , finite (tiers :: [[ Bool ]])  == True
  , finite (tiers :: [[ (Bool,Bool) ]]) == True
  , finite (tiers :: [[ Nat1 ]])  == True
  , finite (tiers :: [[ Nat2 ]])  == True
  , finite (tiers :: [[ Nat3 ]])  == True
  , finite (tiers :: [[ Nat4 ]])  == True
  , finite (tiers :: [[ Nat5 ]])  == True
  , finite (tiers :: [[ Nat6 ]])  == True
  , finite (tiers :: [[ Nat7 ]])  == True
  , finite (tiers :: [[ Word1 ]])  == True
  , finite (tiers :: [[ Word2 ]])  == True
  , finite (tiers :: [[ Word3 ]])  == True

  , finite (tiers :: [[ Nat ]])   == False
  , finite (tiers :: [[ Int ]])   == False
  , finite (tiers :: [[ [Int] ]]) == False
  , finite (tiers :: [[ [()] ]])  == False

  -- false negatives, more than 12 values:
  , finite (tiers :: [[ Word4 ]])  == False
  , finite (tiers :: [[ (Bool,Bool,Bool,Bool,Bool) ]]) == False
  , finite (tiers :: [[ (Bool,Bool,Bool,Bool,Bool,Bool) ]]) == False

  , holds n $ \xss -> ordered . concat $ discardLaterT (<)  (xss::[[Int]])
  , holds n $ \xss -> ordered . concat $ discardLaterT (<=) (xss::[[Int]])
  , (length . concat $ discardLaterT (<=) [[1..100]]) == 100
  , (length . concat $ discardLaterT (<=) [[00..99],[100..199::Int]]) == 200
  , holds n $ \xss -> nub (concat xss) == concat (nubT xss :: [[Int]])
  ]

deleteTIsMapDelete :: (Eq a, Listable a) => Int -> a -> Bool
deleteTIsMapDelete n x  =
  deleteT x tiers =| n |= normalizeT (map (delete x) tiers)

checkNoDup :: Int -> Bool
checkNoDup n  =
  noDupListsOf (tiers :: [[Int]]) =| n |= tiers `suchThat` noDup
  where
  noDup xs  =  nub (sort xs) == sort xs

checkBags :: Int -> Bool
checkBags n  =
  bagsOf (tiers :: [[Nat]]) =| n |= tiers `suchThat` ordered

checkSets :: Int -> Bool
checkSets n  =
  setsOf (tiers :: [[Nat]]) =| n |= tiers `suchThat` strictlyOrdered

checkDistinctPairs :: Int -> Bool
checkDistinctPairs n  =
  distinctPairs (tiers :: [[Nat]]) =| n |= tiers `suchThat` uncurry (/=)

checkUnorderedDistinctPairs :: Int -> Bool
checkUnorderedDistinctPairs n  =
  unorderedDistinctPairs (tiers :: [[Nat]]) =| n |= tiers `suchThat` uncurry (<)

checkUnorderedPairs :: Int -> Bool
checkUnorderedPairs n  =
  unorderedPairs (tiers :: [[Nat]]) =| n |= tiers `suchThat` uncurry (<=)

checkLengthListingsOfLength :: Int -> Int -> Bool
checkLengthListingsOfLength n m  =  all check [1..m]
  where check m  =  all (\xs -> length xs == m)
                 $  concat . take n
                 $  listsOfLength m natTiers

checkSizesListingsOfLength :: Int -> Int -> Bool
checkSizesListingsOfLength n m  =  all check [1..m]
  where check m  =  orderedBy compare
                 $  map sum . concat . take n
                 $  listsOfLength m natTiers

natTiers :: [[Nat]]
natTiers  =  tiers
