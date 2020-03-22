{-# LANGUAGE CPP #-}
-- |
-- Module      : Test.LeanCheck.Stats
-- Copyright   : (c) 2017-2020 Rudy Matela
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
#ifndef __HUGS__
import Data.List (intercalate, transpose)
#else
import Data.List (transpose)

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)
  where
  intersperse             :: a -> [a] -> [a]
  intersperse _   []      = []
  intersperse sep (x:xs)  = x : prependToAll sep xs
    where
    prependToAll            :: a -> [a] -> [a]
    prependToAll _   []     = []
    prependToAll sep (x:xs) = sep : x : prependToAll sep xs
#endif

-- | Prints statistics about a given 'Listable' generator
--   up to a number of test values.
--
-- For example,
-- to know the distribution of the length of generated lists,
-- just run:
--
-- > > classStats 100 (length :: [Int] -> Int)
-- > 0:  1/100  1%
-- > 1:  6/100  6%
-- > 2: 16/100 16%
-- > 3: 25/100 25%
-- > 4: 26/100 26%
-- > 5: 18/100 18%
-- > 6:  7/100  7%
-- > 7:  1/100  1%
--
-- This provides similar functionality to QuickCheck's
-- label, collect, classify and tabulate
-- while keeping statistics separate from your properties.
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

-- | Same as 'classStats', but separated by 'tiers'.
--
-- The first argument is the number of tiers to evaluate.
--
-- > > classStatsT 6 (length :: [Int] -> Int)
-- >       tot   0  1  2  3  4   5
-- > 
-- > tot:   32   1  1  2  4  8  16
-- >   0:    1   1  0  0  0  0   0
-- >   1:    5   0  1  1  1  1   1
-- >   2:   10   0  0  1  2  3   4
-- >   3:   10   0  0  0  1  3   6
-- >   4:    5   0  0  0  0  1   4
-- >   5:    1   0  0  0  0  0   1
--
-- Lines are values, columns are tiers:
--
-- > > classStatsT 6 and
-- >         tot   0  1  2  3   4   5
-- > 
-- >   tot:   63   1  2  4  8  16  32
-- >  True:    6   1  1  1  1   1   1
-- > False:   57   0  1  3  7  15  31
classStatsT :: (Listable a, Show b) => Int -> (a -> b) -> IO ()
classStatsT n f = putStrLn
                . table "  "
                . (heading:)
                . ([" "]:)
                . map showCounts
                . prependTotal
                . countsTOn (unquote . show . f)
                $ take n tiers
  where
  heading = "" : "tot " : map show [0..(n-1)]
  showCounts (s,n,ns) = (s ++ ":") : (show n ++ " ") : map show ns
  (_,n,ns) -+- (_,n',ns') = ("tot", n + n', zipWith (+) ns ns')
  totalizeCounts = foldr (-+-) (undefined, 0, repeat 0)
  prependTotal cs = totalizeCounts cs : cs

-- | How many values match each of a list of conditions?
-- 
-- How many odd and even numbers are in the 'Listable' enumeration?
--
-- > > conditionStats 1000 [("odd", odd :: Int -> Bool), ("even", even)]
-- >  odd: 500/1000 50%
-- > even: 500/1000 50%
--
-- How many of the generated lists are ordered?
--
-- > > conditionStats 1000 [("ordered", ordered :: [Int] -> Bool)]
-- > ordered: 131/1000 13%
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

-- | Same as 'conditionStats' but by tier.
--
-- The first argument is the number of tiers to evaluate.
--
-- How many odd and even numbers are in each tier?
--
-- > > conditionStatsT 10 [("odd", odd :: Int -> Bool), ("even", even)]
-- > total: 1 1 1 1 1 1 1 1 1 1
-- >   odd: 0 1 1 0 0 1 1 0 0 1
-- >  even: 1 0 0 1 1 0 0 1 1 0
--
-- How many ordered lists are in each tier?
--
-- > > conditionStatsT 10 [("ordered", ordered :: [Int] -> Bool)]
-- >   total: 1 1 2 4 8 16 32 64 128 256
-- > ordered: 1 1 2 3 5  7 11 15  22  30
conditionStatsT :: Listable a => Int -> [(String,a->Bool)] -> IO ()
conditionStatsT n = putStrLn . table " " . map show1 . (("total", const True):)
  where
  xss = take n tiers
  show1 (s,f) = (s ++ ":") : map (show . count f) xss
  count f = length . filter f

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

countsT :: Eq a => [[a]] -> [(a,Int,[Int])]
countsT xss = [(x,n,map (count x) xss) | (x,n) <- counts (concat xss)]
  where
  count x = length . filter (== x)

countsTOn :: Eq b => (a -> b) -> [[a]] -> [(b,Int,[Int])]
countsTOn f = countsT . mapT f

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
