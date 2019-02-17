-- Copyright (c) 2015-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import System.Exit (exitFailure)
import Data.List (elemIndices)

-- import Test.LeanCheck -- already exported by Test
-- import Test.LeanCheck.Utils


main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests :: [Bool]
tests =
  [ True
  -- TODO: add some tests here
  , holds 100 $ \() -> True
  ]
