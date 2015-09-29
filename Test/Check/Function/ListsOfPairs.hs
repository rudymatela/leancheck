-- | Function enumeration via lists of pairs.
module Test.Check.Function.ListsOfPairs () where

import Test.Check
import Test.Check.Utils

instance (Eq a, Listable a, Listable b) => Listable (a -> b) where
  listing = lsmap (uncurry $ flip defaultPairsToFunction)
          $ lsFunctions list listing


lsFunctions :: [[a]] -> [[b]] -> [[([(a,b)],b)]]
lsFunctions xss yss =
  lsConcatMap
    (\(r,yss) -> lsmap (\ps -> (ps,r)) $ lsFunctionPairs xss yss)
    (djs yss)
