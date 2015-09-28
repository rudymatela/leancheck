import System.Exit (exitFailure)
import Data.List (elemIndices, sort, nub)

import Test.Check
import Test.Check.Utils
import Test.Check.Invariants
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
  ]

-- TODO: Remove map reverse (make actual code consistent)
checkNoDup :: Int -> Bool
checkNoDup n = take n (lsNoDupListsOf (listing :: [[Int]]))
            == take n ((map . filter) noDup (map reverse $ listing :: [[[Int]]]))
  where noDup xs = nub (sort xs) == sort xs

-- TODO: Note: this will fail when a proper Int enumeration is activated (use
-- Nats here)
checkCrescent :: Int -> Bool
checkCrescent n = take n (lsNoDecListsOf (listing :: [[Nat]]))
               == take n ((map . filter) (strictlyOrderedBy compare) (map reverse $ listing :: [[[Nat]]]))
