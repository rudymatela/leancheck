-- |
-- Module      : Test.LeanCheck.Function.ListsOfPairs
-- Copyright   : (c) 2015-2020 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports means to enumerate functions via lists of pairs.
--
-- This module considers functions as a finite list of exceptional input-output
-- cases to a default value (list of pairs of arguments and results).
module Test.LeanCheck.Function.ListsOfPairs
  ( (-->>)
  , exceptionPairs
  )
where

import Test.LeanCheck
import Test.LeanCheck.Tiers

-- | Given tiers of argument and result values,
--   return tiers of functional values.
(-->>) :: Eq a => [[a]] -> [[b]] -> [[a->b]]
xss -->> yss
  | finite xss  =  mapT ((undefined `mutate`) . zip (concat xss))
                        (products $ replicate (length $ concat xss) yss)
  | otherwise   =  concatMapT (\(r,yss) -> mapT (const r `mutate`)
                                                (exceptionPairs xss yss))
                              (choices yss)


mutate :: Eq a => (a -> b) -> [(a,b)] -> (a -> b)
mutate f ms  =  foldr mut f ms
  where
  mut (x',fx') f x  =  if x == x' then fx' else f x


-- | Given tiers of input values and tiers of output values,
-- return tiers with all possible lists of input-output pairs.
-- These represent functional relations.
-- In the implementation of '-->>',
-- they represent exceptions to a constant function,
-- hence the name 'exceptionPairs'.
exceptionPairs :: [[a]] -> [[b]] -> [[ [(a,b)] ]]
exceptionPairs xss yss  =  concatMapT exceptionsFor (incompleteSetsOf xss)
  where
--exceptionsFor :: [a] -> [[ [(a,b)] ]]
  exceptionsFor xs  =  zip xs `mapT` products (const yss `map` xs)
-- incompleteSetsOf is needed, instead of setsOf, because mutating *all* values
-- of a constant function makes no sense (we would have already enumerated that
-- function anyway).  As of 2c23c1a, it makes no difference whether
-- incompleteSetsOf is used instead of setsOf for types with less than 12
-- values because of the finite guard on `-->>`.

-- | Returns tiers of sets excluding the universe set.
--
-- > incompleteSetsOf (tiers :: [[Bool]])  =  [[],[[False],[True]],[]]
-- > incompleteSetsOf (tiers :: [[()]])    =  [[]]
--
-- This is the same as 'setsOf' on types with infinite values:
--
-- > incompleteSetsOf (tiers :: [[Int]])  =  setsOf (tiers :: [[Int]])
incompleteSetsOf :: [[a]] -> [[ [a] ]]
incompleteSetsOf  =  init . setsOf
-- the above implementation works because, and depends on the fact that:
-- the last tier returned by setsOf contains only the complete set
