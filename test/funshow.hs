-- Copyright (c) 2015-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
import System.Exit (exitFailure)
import Data.List (elemIndices,sort)
import Test.LeanCheck.Function.ShowFunction
import Test.LeanCheck.Function.Show ()
import Test.LeanCheck.Utils.TypeBinding

main :: IO ()
main =
  case elemIndices False (tests 200) of
    [] -> putStrLn "Tests passed!"
    is -> do putStrLn ("Failed tests:" ++ show is)
             exitFailure

tests :: Int -> [Bool]
tests _ =
  [ True

  -- undefined values --
  , showFunction 2 int == "undefined"
  , showFunction 2 bool == "undefined"
  , showFunction 2 char == "undefined"
  , showFunctionLine 10 (int  >- int) == "\\_ -> undefined"
  , showFunctionLine 10 (bool >- bool) == "\\_ -> undefined"

  -- partially defined --
  , showFunctionLine 2 (\x -> case x of True -> True)
                   == "\\x -> case x of True -> True"
  , showFunctionLine 10 (\x -> case (x::Int) of 3 -> (4::Int); 5 -> 6)
                    == "\\x -> case x of 3 -> 4; 5 -> 6"
  , showFunctionLine 10 (\x y -> case (x::Int,y::Int) of (1,2) -> (3::Int); (2,1) -> 3)
                    == "\\x y -> case (x,y) of (1,2) -> 3; (2,1) -> 3"

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
  , show not
    == "\\x -> case x of\n"
    ++ "      False -> True\n"
    ++ "      True -> False\n"
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
    ++ "        (True,True) -> True\n"
    ++ "        _ -> False\n"
  , showFunction 4 (&&)
    == "\\x y -> case (x,y) of\n"
    ++ "        (True,True) -> True\n"
    ++ "        _ -> False\n"
  , showFunction 3 (||)
    == "\\x y -> case (x,y) of\n"
    ++ "        (False,False) -> False\n"
    ++ "        _ -> True\n"
  , showFunction 4 (||)
    == "\\x y -> case (x,y) of\n"
    ++ "        (False,False) -> False\n"
    ++ "        _ -> True\n"

  , showFunction 4 arg1of1is0
    == "\\x -> case x of\n"
    ++ "      0 -> True\n"
    ++ "      _ -> False\n"
  , showFunction 4 arg1of1isnt1
    == "\\x -> case x of\n"
    ++ "      1 -> False\n"
    ++ "      _ -> True\n"

  , showFunction 4 arg1of2is0
    == "\\x _ -> case x of\n"
    ++ "        0 -> True\n"
    ++ "        _ -> False\n"
  , showFunction 4 arg2of2is0
    == "\\_ y -> case y of\n"
    ++ "        0 -> True\n"
    ++ "        _ -> False\n"
  , showFunction 4 arg2of3isnt2
    == "\\_ y _ -> case y of\n"
    ++ "          2 -> False\n"
    ++ "          _ -> True\n"
  , showFunction 4 args1or3of3are0
    == "\\x _ z -> case (x,z) of\n"
    ++ "          (0,_) -> True\n"
    ++ "          (_,0) -> True\n"
    ++ "          _ -> False\n"
  , showFunction 4 args13of3arent0
    == "\\x _ z -> case (x,z) of\n"
    ++ "          (0,0) -> False\n"
    ++ "          _ -> True\n"
  ]

arg1of1is0 :: Int -> Bool
arg1of1is0 =
  \x -> case x of
        0 -> True
        _ -> False

arg1of1isnt1 :: Int -> Bool
arg1of1isnt1 =
  \x -> case x of
        1 -> False
        _ -> True

arg1of2is0 :: Int -> Int -> Bool
arg1of2is0 =
  \x y -> case (x,y) of
          (0,_) -> True
          _ -> False

arg2of2is0 :: Int -> Int -> Bool
arg2of2is0 =
  \x y -> case (x,y) of
          (_,0) -> True
          _ -> False

arg2of3isnt2 :: Int -> Int -> Int -> Bool
arg2of3isnt2 =
  \x y z -> case (x,y,z) of
            (_,2,_) -> False
            _ -> True

args1or3of3are0 :: Int -> Int -> Int -> Bool
args1or3of3are0 =
  \x y z -> case (x,y,z) of
            (0,_,_) -> True
            (_,_,0) -> True
            _ -> False

args13of3arent0 :: Int -> Int -> Int -> Bool
args13of3arent0 =
  \x y z -> case (x,y,z) of
            (0,_,0) -> False
            _ -> True
