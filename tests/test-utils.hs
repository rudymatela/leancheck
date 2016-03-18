import System.Exit (exitFailure)
import Data.List (elemIndices, sort, nub)

import Test.Check
import Test.Check.Invariants
import Test.Operators
import Test.TypeBinding
import Test.Types (Nat)


main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests =
  [ True

  , checkNoDup 12
  , checkCrescent 20
  , checkLengthListingsOfLength 5 5
  , checkSizesListingsOfLength 5 5

  , productMaybeWith ($) [[const Nothing, Just]] [[1],[2],[3],[4]] == [[1],[2],[3],[4]]
  , productMaybeWith (flip ($))
                     [[1],[2],[3],[4]]
                     [[const Nothing],[Just]] == [[],[1],[2],[3],[4]]

  , holds 100 $ ptofApp -:> (char,char)
  , holds 100 $ associationsValues int  100 -:> [int]
  , holds 100 $ associationsValues bool 100 -:> [bool]
  , holds 500 $ associationsNewAndOld -: [int]  >- [[int]]  >- und
  , holds 500 $ associationsNewAndOld -: [int]  >- [[bool]] >- und
  , holds 500 $ associationsNewAndOld -: [bool] >- [[bool]] >- und
  , holds 500 $ associationsNewAndOld -: [bool] >- [[int]]  >- und
  ]

checkNoDup :: Int -> Bool
checkNoDup n = noDupListsOf (tiers :: [[Int]])
       =| n |= (map . filter) noDup (tiers :: [[[Int]]])
  where noDup xs = nub (sort xs) == sort xs

checkCrescent :: Int -> Bool
checkCrescent n = setsOf (tiers :: [[Nat]])
          =| n |= (map . filter) strictlyOrdered (tiers :: [[[Nat]]])

checkLengthListingsOfLength :: Int -> Int -> Bool
checkLengthListingsOfLength n m = all check [1..m]
  where check m = all (\xs -> length xs == m)
                $ concat . take n
                $ listsOfLength m natTiers

checkSizesListingsOfLength :: Int -> Int -> Bool
checkSizesListingsOfLength n m = all check [1..m]
  where check m = orderedBy compare
                $ map sum . concat . take n
                $ listsOfLength m natTiers

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
  where oldAssociations xs sbs = tmap (zip xs) (listsOfLength (length xs) sbs)


natTiers :: [[Nat]]
natTiers = tiers

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique (filter (< x) xs)
                && allUnique (filter (> x) xs)
