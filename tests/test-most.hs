-- Simple file just to test if everything is imported fine
import System.Exit (exitFailure)
import Data.List (elemIndices)

import Test.Most
import Test.Check.Invariants (strictlyOrdered)

main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests =
  [ True

  -- Test.Check
  , holds 1 True

  -- Test.Check.Utils
  , checkCrescent 1

  -- Test.Types
  , [minBound..maxBound :: UInt1] == [0,1]

  -- Test.Operators
  , holds 1 $ (not . not) === id
  ]

checkCrescent :: Int -> Bool
checkCrescent n = take n (lsStrictlyAscendingListsOf (listing :: [[Nat]]))
               == take n ((map . filter) strictlyOrdered (listing :: [[[Nat]]]))

