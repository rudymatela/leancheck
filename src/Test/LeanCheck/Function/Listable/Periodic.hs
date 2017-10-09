-- |
-- Module      : Test.LeanCheck.Function.Periodic
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports a 'Listable' instance for enumeration of periodic
-- functions.
--
-- This module /barely works/ and is just a sketch.
module Test.LeanCheck.Function.Listable.Periodic
where


import Test.LeanCheck
import Data.List (inits)


instance (Eq a, Eq b, Listable a, Listable b) => Listable (a -> b) where
  tiers = mapT pairsToFunction $ functions list tiers

functions :: Eq b => [a] -> [[b]] -> [[[(a,b)]]]
functions xs yss = mapT (zip xs . cycle) $ lsPeriodsOfLimit xs yss

functionsz :: Eq b => [[a]] -> [[b]] -> [[[(a,b)]]]
functionsz xss = functions (concat xss)


lsPeriodsOf :: Eq a => [[a]] -> [[[a]]]
lsPeriodsOf xss = map (filter isPeriod) (listsOf xss)

lsPeriodsOfLimit :: Eq a => [b] -> [[a]] -> [[[a]]]
lsPeriodsOfLimit ys xss = map (filter isPeriod) (tiersOfLimit ys xss)


isPeriod :: Eq a => [a] -> Bool
isPeriod [] = False
isPeriod [x] = True
isPeriod xs = not $ any (`isPeriodOf` xs) $ (tail . init . inits) xs

isPeriodOf :: Eq a => [a] -> [a] -> Bool
xs `isPeriodOf` ys = length ys `mod` length xs == 0
                  && and (zipWith (==) (cycle xs) ys)


tiersOfLimit :: [b] -> [[a]] -> [[[a]]]
tiersOfLimit     [] xss = [[[]]]
tiersOfLimit (_:ys) xss = [[[]]] ++ productWith (:) xss (tiersOfLimit ys xss)


pairsToFunction :: Eq a => [(a,b)] -> (a -> b)
pairsToFunction ((x,y):ps) x' =  if x' == x
                                   then y
                                   else pairsToFunction ps x'
