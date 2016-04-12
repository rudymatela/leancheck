-- Simple file just to test if everything is imported fine
import System.Exit (exitFailure)
import Data.List (elemIndices)

import Test.LeanCheck.Most
import Test.LeanCheck.Invariants (strictlyOrdered)

main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests =
  [ True

  -- Test.LeanCheck
  , holds 1 True

  -- Test.LeanCheck.Utils
  , checkCrescent 1

  -- Test.LeanCheck.Types
  , [minBound..maxBound :: UInt1] == [0,1]

  -- Test.LeanCheck.Operators
  , holds 1 $ (not . not) === id
  ]

checkCrescent :: Int -> Bool
checkCrescent n = strictlyAscendingListsOf (tiers :: [[Nat]])
          =| n |= (map . filter) strictlyOrdered (tiers :: [[[Nat]]])

