-- tiers.hs -- prints tiers of values up to a certain point
--
-- Copyright (c) 2015-2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Test.LeanCheck.Utils.Types
import Test.LeanCheck.Function
import System.Environment

beside :: String -> String -> String
beside heading s = init $ unlines $
  zipWith
    (++)
    ([heading] ++ repeat (replicate (length heading) ' '))
    (lines s)

listLines :: [String] -> String
listLines []  = "[]"
listLines [s] | '\n' `notElem` s = "[" ++ s ++ "]"
listLines ss  = (++ "]")
              . unlines
              . zipWith beside (["[ "] ++ repeat ", ")
              $ ss

showListLines :: Show a => [a] -> String
showListLines = listLines . map show

showTiersLines :: Show a => [[a]] -> String
showTiersLines = listLines . map showListLines

putTiers :: Show a => Int -> [[a]] -> IO ()
putTiers n = putStrLn . ("  " `beside`) . showTiersLines . take n

main :: IO ()
main = do
  as <- getArgs
  let (t,n) = case as of
                []    -> ("Int", 12)
                [t]   -> (t,     12)
                [t,n] -> (t, read n)
  putStrLn $ "tiers :: [" ++ t ++ "]  ="
  case t of
    -- simple types
    "()"            -> put n (u :: ()            )
    "Int"           -> put n (u :: Int           )
    "Nat"           -> put n (u :: Nat           )
    "Integer"       -> put n (u :: Integer       )
    "Bool"          -> put n (u :: Bool          )
    "Char"          -> put n (u :: Char          )
    -- lists
    "[()]"          -> put n (u :: [()]          )
    "[Int]"         -> put n (u :: [Int]         )
    "[Nat]"         -> put n (u :: [Nat]         )
    "[Integer]"     -> put n (u :: [Integer]     )
    "[Bool]"        -> put n (u :: [Bool]        )
    "[Char]"        -> put n (u :: [Char]        )
    "String"        -> put n (u :: String        )
    -- pairs
    "((),())"       -> put n (u :: ((),())       )
    "(Int,Int)"     -> put n (u :: (Int,Int)     )
    "(Nat,Nat)"     -> put n (u :: (Nat,Nat)     )
    "(Bool,Bool)"   -> put n (u :: (Bool,Bool)   )
    "(Bool,Int)"    -> put n (u :: (Bool,Int)    )
    "(Int,Bool)"    -> put n (u :: (Int,Bool)    )
    "(Int,Int,Int)" -> put n (u :: (Int,Int,Int) )
    "(Nat,Nat,Nat)" -> put n (u :: (Nat,Nat,Nat) )
    -- lists & pairs
    "[((),())]"       -> put n (u :: [((),())]       )
    "([()],[()])"     -> put n (u :: ([()],[()])     )
    "([Bool],[Bool])" -> put n (u :: ([Bool],[Bool]) )
    "([Int],[Int])"   -> put n (u :: ([Int],[Int])   )
    -- functions
    "Int->Int"      -> put n (u :: Int -> Int    )
    "Nat->Nat"      -> put n (u :: Nat -> Nat    )
    -- unhandled
    _               -> putStrLn $ "unknown/unhandled type `" ++ t ++ "'"
  where
  u :: a
  u = undefined
  put :: (Show a, Listable a) => Int -> a -> IO ()
  put n a = putTiers n (tiers `asTypeOf` [[a]])
