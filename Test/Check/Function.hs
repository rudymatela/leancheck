-- | Function enumeration via lists of pairs.
module Test.Check.Function () where

import Test.Check
import Test.Check.Utils

instance (Eq a, Listable a, Listable b) => Listable (a -> b) where
  listing = lsmap totalBindingsToFunction $ lsFunctions list listing


