-- pick.hs -- picks the n-th value in a LeanCheck enumeration
--
-- Copyright (c) 2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.LeanCheck.Function ()

import System.Environment

u :: a
u  =  undefined

usage :: IO ()
usage  =  putStrLn $ "usage:"
                  \\ "  pick <Type> <n>"
                  \\ ""
                  \\ "example:"
                  \\ "  pick Int 12"
  where
  s1 \\ s2  =  s1 ++ "\n" ++ s2

main :: IO ()
main  =  do
  as <- getArgs
  case as of
    [t,n] -> pick t (read n)
    _ -> usage

put :: (Listable a, Show a) => String -> Int -> a -> IO ()
put t n u  =  putStrLn
           $  "list :: [" ++ t ++ "] !! " ++ show n ++ " = "
           ++ show ((list `asTypeOf` [u]) !! n)

pick :: String -> Int -> IO ()
pick t n = case t of
  "()"               -> put t n (u :: ()                   )
  "Int"              -> put t n (u :: Int                  )
  "Nat"              -> put t n (u :: Nat                  )
  "Integer"          -> put t n (u :: Integer              )
  "Bool"             -> put t n (u :: Bool                 )
  "Char"             -> put t n (u :: Char                 )
  "Float"            -> put t n (u :: Float                )
  "Double"           -> put t n (u :: Double               )
  "Rational"         -> put t n (u :: Rational             )
  -- lists
  "[()]"             -> put t n (u :: [()]                 )
  "[Int]"            -> put t n (u :: [Int]                )
  "[Nat]"            -> put t n (u :: [Nat]                )
  "[Integer]"        -> put t n (u :: [Integer]            )
  "[Bool]"           -> put t n (u :: [Bool]               )
  "[Char]"           -> put t n (u :: [Char]               )
  "String"           -> put t n (u :: String               )
  -- pairs
  "((),())"          -> put t n (u :: ((),())              )
  "(Int,Int)"        -> put t n (u :: (Int,Int)            )
  "(Nat,Nat)"        -> put t n (u :: (Nat,Nat)            )
  "(Bool,Bool)"      -> put t n (u :: (Bool,Bool)          )
  "(Bool,Int)"       -> put t n (u :: (Bool,Int)           )
  "(Int,Bool)"       -> put t n (u :: (Int,Bool)           )
  "(Int,Int,Int)"    -> put t n (u :: (Int,Int,Int)        )
  "(Nat,Nat,Nat)"    -> put t n (u :: (Nat,Nat,Nat)        )
  -- lists & pairs
  "[((),())]"        -> put t n (u :: [((),())]            )
  "([()],[()])"      -> put t n (u :: ([()],[()])          )
  "([Bool],[Bool])"  -> put t n (u :: ([Bool],[Bool])      )
  "([Int],[Int])"    -> put t n (u :: ([Int],[Int])        )
  -- lists of lists
  "[[()]]"           -> put t n (u :: [[()]]               )
  "[[Int]]"          -> put t n (u :: [[Int]]              )
  "[[Nat]]"          -> put t n (u :: [[Nat]]              )
  "[[Integer]]"      -> put t n (u :: [[Integer]]          )
  "[[Bool]]"         -> put t n (u :: [[Bool]]             )
  "[[Char]]"         -> put t n (u :: [[Char]]             )
  "[String]"         -> put t n (u :: [String]             )
  -- lists of pairs
  "[(Int,Int)]"      -> put t n (u :: [(Int,Int)]          )
  -- unhandled
  _                  -> error $ "unknown/unhandled type `" ++ t ++ "'"
