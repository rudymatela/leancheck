-- | Function enumeration via lists of pairs.
module Test.Check.Function.ListsOfPairs () where

import Test.Check.Core
import Test.Check.Utils

instance (Eq a, Listable a, Listable b) => Listable (a -> b) where
  tiers = tmap (uncurry $ flip defaultPairsToFunction)
        $ functions list tiers


functions :: [[a]] -> [[b]] -> [[([(a,b)],b)]]
functions xss yss =
  concatMapT
    (\(r,yss) -> tmap (\ps -> (ps,r)) $ functionPairs xss yss)
    (choices yss)
