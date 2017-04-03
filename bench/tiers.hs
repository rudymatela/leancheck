-- tiers.hs -- prints tiers of values up to a certain point
--
-- Copyright (c) 2015-2017 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Test.LeanCheck.Utils.Types
import Test.LeanCheck.Function
import System.Environment

putTiers :: Show a => Int -> [[a]] -> IO ()
putTiers n = putStr . unlines . take n . map (unlines . map show)

main :: IO ()
main = do
  as <- getArgs
  let (t,n) = case as of
                []    -> ("Int", 12)
                [t]   -> (t,     12)
                [t,n] -> (t, read n)
  case t of
    "Int"         -> putTiers n (tiers :: [[Int]])
    "Nat"         -> putTiers n (tiers :: [[Nat]])
    "Integer"     -> putTiers n (tiers :: [[Integer]])
    "Bool"        -> putTiers n (tiers :: [[Bool]])
    "(Int,Int)"   -> putTiers n (tiers :: [[(Int,Int)]])
    "(Bool,Bool)" -> putTiers n (tiers :: [[(Bool,Bool)]])
    "(Bool,Int)"  -> putTiers n (tiers :: [[(Bool,Int)]])
    "(Int,Bool)"  -> putTiers n (tiers :: [[(Int,Bool)]])
    "()"          -> putTiers n (tiers :: [[()]])
    _             -> putStrLn $ "unknown/unhandled type `" ++ t ++ "'"
