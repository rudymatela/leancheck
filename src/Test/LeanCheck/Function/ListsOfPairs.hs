-- |
-- Module      : Test.LeanCheck.Function.ListsOfPairs
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports a 'Listable' instance for function enumeration
-- via lists of pairs.
--
-- This module considers functions as a finite list of exceptional input-output
-- cases to a default value (list of pairs of arguments and results).
module Test.LeanCheck.Function.ListsOfPairs
  ( functionPairs
  , associations
  , pairsToFunction
  , defaultFunPairsToFunction
  )
where

import Test.LeanCheck
import Test.LeanCheck.Tiers
import Data.Maybe (fromMaybe)

instance (Eq a, Listable a, Listable b) => Listable (a -> b) where
  tiers = mapT (uncurry $ flip defaultPairsToFunction)
        $ functions list tiers


functions :: [[a]] -> [[b]] -> [[([(a,b)],b)]]
functions xss yss =
  concatMapT
    (\(r,yss) -> mapT (\ps -> (ps,r)) $ functionPairs xss yss)
    (choices yss)


-- | Given a list of domain values, and tiers of codomain values,
-- return tiers of lists of ordered pairs of domain and codomain values.
--
-- Technically: tiers of left-total functional relations.
associations :: [a] -> [[b]] -> [[ [(a,b)] ]]
associations xs sbs = zip xs `mapT` products (const sbs `map` xs)

-- | Given tiers of input values and tiers of output values,
-- return tiers with all possible lists of input-output pairs.
-- Those represent functional relations.
functionPairs :: [[a]] -> [[b]] -> [[[(a,b)]]]
functionPairs xss yss = concatMapT (`associations` yss)
                                   (setsOf xss)

-- | Returns a function given by a list of input-output pairs.
-- The result is wrapped in a maybe value.
-- The output for bound inputs is 'Just' a value.
-- The output for unbound inputs is 'Nothing'.
pairsToMaybeFunction :: Eq a => [(a,b)] -> a -> Maybe b
pairsToMaybeFunction []          _ = Nothing
pairsToMaybeFunction ((a',r):bs) a | a == a'   = Just r
                                   | otherwise = pairsToMaybeFunction bs a

-- | Returns a partial function given by a list of input-output pairs.
--
-- NOTE: This function *will* return undefined values for unbound inputs.
pairsToFunction :: Eq a => [(a,b)] -> a -> b
pairsToFunction bs a = fromMaybe undefined (pairsToMaybeFunction bs a)


-- | Returns a function given by a list of input-output pairs and a default value.
defaultPairsToFunction :: Eq a => b -> [(a,b)] -> a -> b
defaultPairsToFunction r bs a = fromMaybe r (pairsToMaybeFunction bs a)


defaultFunPairsToFunction :: Eq a => (a -> b) -> [(a,b)] -> a -> b
defaultFunPairsToFunction f bs a = fromMaybe (f a) (pairsToMaybeFunction bs a)
