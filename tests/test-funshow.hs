-- Copyright (c) 2015-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import System.Exit (exitFailure)
import Data.List (elemIndices,sort)
import Test.LeanCheck.Function.ShowFunction
import Test.LeanCheck.Function.Show
import Test.LeanCheck.Utils.TypeBinding

main :: IO ()
main =
  case elemIndices False tests of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure


tests =
  [ True

  -- undefined values --
  , showFunction 2 int == "undefined"
  , showFunction 2 bool == "undefined"
  , showFunction 2 char == "undefined"
  , showFunctionLine 10 (int  >- int) == "\\x -> undefined"
  , showFunctionLine 10 (bool >- bool) == "\\x -> undefined"

  -- partially defined --
  , showFunctionLine 2 (\x -> case x of True -> True)
                   == "\\x -> case x of True -> True"
  , showFunctionLine 10 (\x -> case (x::Int) of 3 -> (4::Int); 5 -> 6)
                    == "\\x -> case x of 3 -> 4; 5 -> 6; ..."
  , showFunctionLine 10 (\x y -> case (x::Int,y::Int) of (1,2) -> (3::Int); (2,1) -> 3)
                    == "\\x y -> case (x,y) of (1,2) -> 3; (2,1) -> 3; ..."

  -- fully defined, infinite --
  , showFunction 3 ((+) -:> int)
    == "\\x y -> case (x,y) of\n"
    ++ "        (0,0) -> 0\n"
    ++ "        (0,1) -> 1\n"
    ++ "        (1,0) -> 1\n"
    ++ "        ...\n"
  , showFunction 3 ((++) -:> [int])
    == "\\x y -> case (x,y) of\n"
    ++ "        ([],[]) -> []\n"
    ++ "        ([],[0]) -> [0]\n"
    ++ "        ([0],[]) -> [0]\n"
    ++ "        ...\n"
  , showFunction 4 (sort -:> [char])
    == "\\x -> case x of\n"
    ++ "      \"\" -> \"\"\n"
    ++ "      \"a\" -> \"a\"\n"
    ++ "      \"aa\" -> \"aa\"\n"
    ++ "      \" \" -> \" \"\n"
    ++ "      ...\n"


  -- fully defined, finite --
  , showFunction 1 not
    == "\\x -> case x of\n"
    ++ "      False -> True\n"
    ++ "      ...\n"
  , showFunction 2 not
    == "\\x -> case x of\n"
    ++ "      False -> True\n"
    ++ "      True -> False\n"
  , showFunction 3 (&&)
    == "\\x y -> case (x,y) of\n"
    ++ "        (False,False) -> False\n"
    ++ "        (False,True) -> False\n"
    ++ "        (True,False) -> False\n"
    ++ "        ...\n"
  , showFunction 4 (&&)
    == "\\x y -> case (x,y) of\n"
    ++ "        (False,False) -> False\n"
    ++ "        (False,True) -> False\n"
    ++ "        (True,False) -> False\n"
    ++ "        (True,True) -> True\n"
  , showFunction 3 (||)
    == "\\x y -> case (x,y) of\n"
    ++ "        (False,False) -> False\n"
    ++ "        (False,True) -> True\n"
    ++ "        (True,False) -> True\n"
    ++ "        ...\n"
  , showFunction 4 (||)
    == "\\x y -> case (x,y) of\n"
    ++ "        (False,False) -> False\n"
    ++ "        (False,True) -> True\n"
    ++ "        (True,False) -> True\n"
    ++ "        (True,True) -> True\n"
  ]
