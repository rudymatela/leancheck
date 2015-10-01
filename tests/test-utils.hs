import System.Exit (exitFailure)
import Data.List (elemIndices, sort, nub)

import Test.Check
import Test.Check.Utils
import Test.Check.Invariants
import Test.Types (Nat)


(==>) :: Bool -> Bool -> Bool
False ==> _ = True
_     ==> y = y
infixr 0 ==>

argTypeOf :: (a -> b) -> a -> (a -> b)
argTypeOf = const

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

  , holds 100 $ ptofApp `argTypeOf` ('a','b')
  , holds 100 $ associationsValues (undefined::Int)  100 `argTypeOf` [undefined::Int]
  , holds 100 $ associationsValues (undefined::Bool) 100 `argTypeOf` [undefined::Bool]
  , holds 500 $ associationsNewAndOld `asTypeOf` (undefined :: [Int] -> [[Int]] -> Bool)
  , holds 500 $ associationsNewAndOld `asTypeOf` (undefined :: [Int] -> [[Bool]] -> Bool)
  , holds 500 $ associationsNewAndOld `asTypeOf` (undefined :: [Bool] -> [[Bool]] -> Bool)
  , holds 500 $ associationsNewAndOld `asTypeOf` (undefined :: [Bool] -> [[Int]] -> Bool)
  ]

-- TODO: Remove map reverse (make actual code consistent)
checkNoDup :: Int -> Bool
checkNoDup n = take n (lsNoDupListsOf (listing :: [[Int]]))
            == take n ((map . filter) noDup (map reverse $ listing :: [[[Int]]]))
  where noDup xs = nub (sort xs) == sort xs

checkCrescent :: Int -> Bool
checkCrescent n = take n (lsCrescListsOf (listing :: [[Nat]]))
               == take n ((map . filter) (strictlyOrderedBy compare) (map reverse $ listing :: [[[Nat]]]))

checkLengthListingsOfLength :: Int -> Int -> Bool
checkLengthListingsOfLength n m = all check [1..m]
  where check m = all (\xs -> length xs == m)
                $ concat . take n
                $ listingsOfLength m natListing

checkSizesListingsOfLength :: Int -> Int -> Bool
checkSizesListingsOfLength n m = all check [1..m]
  where check m = orderedBy compare
                $ map sum . concat . take n
                $ listingsOfLength m natListing

ptofApp :: (Ord a, Eq b) => (a,b) -> [(a,b)] -> Bool -- Ord a is just for allUnique
ptofApp (x,y) ps = (x,y) `elem` ps && allUnique (map fst ps)
               ==> pairsToFunction ps x == y

associationsValues :: (Listable b, Eq a)
                   => b -> Int -> [a] -> Bool
associationsValues ty n xs = all (\xs' -> map fst xs' == xs)
                           $ take n
                           $ concat
                           $ associations xs (listing `asTypeOf` [[ty]])

associationsNewAndOld :: (Eq a, Eq b) => [a] -> [[b]] -> Bool
associationsNewAndOld xs yss = associations xs yss == oldAssociations xs yss
  where oldAssociations xs sbs = lsmap (zip xs) (listingsOfLength (length xs) sbs)


natListing :: [[Nat]]
natListing = listing

allUnique :: Ord a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique (lesser)
                && allUnique (greater)
  where lesser  = filter (< x) xs
        greater = filter (> x) xs
