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
  case t of
    -- simple types
    "()"               -> put t n (u :: ()                   )
    "Int"              -> put t n (u :: Int                  )
    "Nat"              -> put t n (u :: Nat                  )
    "Integer"          -> put t n (u :: Integer              )
    "Bool"             -> put t n (u :: Bool                 )
    "Char"             -> put t n (u :: Char                 )
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
    -- functions
    "()->Bool"         -> put t n (u :: () -> Bool           )
    "Bool->()"         -> put t n (u :: Bool -> ()           )
    "Bool->Bool"       -> put t n (u :: Bool -> Bool         )
    "Bool->Bool->Bool" -> put t n (u :: Bool -> Bool -> Bool )
    "Int->Int"         -> put t n (u :: Int -> Int           )
    "Int->Int->Int"    -> put t n (u :: Int -> Int -> Int    )
    "()->Nat"          -> put t n (u :: () -> Nat            )
    "Nat->()"          -> put t n (u :: Nat -> ()            )
    "Nat->Nat"         -> put t n (u :: Nat -> Nat           )
    "Nat->Nat->Nat"    -> put t n (u :: Nat -> Nat -> Nat    )
    "(Nat,Nat)->Nat"   -> put t n (u :: (Nat,Nat) -> Nat     )
    -- unhandled
    _                  -> putStrLn $ "unknown/unhandled type `" ++ t ++ "'"
  where
  u :: a
  u = undefined
  put :: (Show a, Listable a) => String -> Int -> a -> IO ()
  put t n a = do
    putStrLn $ "tiers :: [" ++ t ++ "]  ="
    putTiers n $ tiers `asTypeOf` [[a]]
    putStrLn $ ""
    putStr $ "map length (tiers :: [" ++ t ++ "])  =  "
          ++ show (map length . take n $ tiers `asTypeOf` [[a]])
