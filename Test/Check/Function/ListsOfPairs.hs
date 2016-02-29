-- | Function enumeration via lists of pairs.
module Test.Check.Function.ListsOfPairs () where

import Test.Check
import Test.Check.Utils

instance (Eq a, Listable a, Listable b) => Listable (a -> b) where
  tiers = tmap (uncurry $ flip defaultPairsToFunction)
        $ tFunctions list tiers


tFunctions :: [[a]] -> [[b]] -> [[([(a,b)],b)]]
tFunctions xss yss =
  tConcatMap
    (\(r,yss) -> tmap (\ps -> (ps,r)) $ tFunctionPairs xss yss)
    (tChoices yss)
