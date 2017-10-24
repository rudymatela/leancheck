-- |
-- Module      : Test.LeanCheck.Stats
-- Copyright   : (c) 2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports functions to compute statistics of Listable instances.
module Test.LeanCheck.Stats
  ( classStats
  , classStatsT
  , conditionStats
  , conditionStatsT

  , classify
  , classifyBy
  , classifyOn
  , counts
  , countsBy
  , countsOn
  )
where

import Test.LeanCheck.Core
import Data.Function (on)
import Data.List (intercalate, transpose)

classStats :: (Listable a, Show b) => Int -> (a -> b) -> IO ()
classStats n f = putStrLn
               . table " "
               . map showCount
               $ countsOn (unquote . show . f) xs
  where
  xs = take n list
  len = length xs
  showCount (s,n) = [ s ++ ":"
                    , show n ++ "/" ++ show len
                    , show (100 * n `div` len) ++ "%"
                    ]

classStatsT :: (Listable a, Show b) => Int -> (a -> b) -> IO ()
classStatsT = error "classStatsT: not implemented yet"

conditionStats :: Listable a => Int -> [(String,a->Bool)] -> IO ()
conditionStats n = putStrLn . table " " . map show1
  where
  xs = take n list
  len = length xs
  show1 (s,f) = let c = count f xs
                in [ s ++ ":"
                   , show c ++ "/" ++ show len
                   , show (100 * c `div` len) ++ "%" ]
  count f = length . filter f

conditionStatsT :: Listable a => Int -> [(String,a->Bool)] -> IO ()
conditionStatsT = error "conditionsStatsT: not implemented yet"

classify :: Eq a => [a] -> [[a]]
classify = classifyBy (==)

classifyBy :: (a -> a -> Bool) -> [a] -> [[a]]
classifyBy (==) []     = []
classifyBy (==) (x:xs) = (x:filter (== x) xs)
                       : classifyBy (==) (filter (/= x) xs)
  where
  x /= y = not (x == y)

classifyOn :: Eq b => (a -> b) -> [a] -> [[a]]
classifyOn f xs = map (map fst)
                . classifyBy ((==) `on` snd)
                $ map (\x -> (x,f x)) xs

counts :: Eq a => [a] -> [(a,Int)]
counts = map headLength . classify

countsBy :: (a -> a -> Bool) -> [a] -> [(a,Int)]
countsBy (==) = map headLength . classifyBy (==)

countsOn :: Eq b => (a -> b) -> [a] -> [(b,Int)]
countsOn f = map (\xs -> (f $ head xs, length xs)) . classifyOn f

headLength :: [a] -> (a,Int)
headLength xs = (head xs, length xs)

unquote :: String -> String
unquote ('"':s) | last s == '"' = init s
unquote s = s

table :: String -> [[String]] -> String
table s []  = ""
table s sss = unlines
            . map (removeTrailing ' ')
            . map (intercalate s)
            . transpose
            . map (normalize ' ')
            . foldr1 (zipWith (++))
            . map (normalize "" . map lines)
            . normalize ""
            $ sss

-- | Fits a list to a certain width by appending a certain value
--
-- > fit ' ' 6 "str" == "str   "
--
-- > fit 0 6 [1,2,3] == [1,2,3,0,0,0]
fit :: a -> Int -> [a] -> [a]
fit x n xs = replicate (n - length xs) x ++ xs

-- | normalize makes all list the same length by adding a value
--
-- > normalize ["asdf","qw","er"] == normalize ["asdf","qw  ","er  "]
normalize :: a -> [[a]] -> [[a]]
normalize x xs = map (x `fit` maxLength xs) xs

-- | Given a list of lists returns the maximum length
maxLength :: [[a]] -> Int
maxLength = maximum . (0:) . map length

removeTrailing :: Eq a => a -> [a] -> [a]
removeTrailing x = reverse
                 . dropWhile (==x)
                 . reverse
