-- | Very simple property-based testing library based on enumeration of values
--   via lists of lists.
--
-- This module exports Most modules that accompain Test.Check
-- and is to be used as a shorthand:
--
-- import Test.Most
--
-- To get most of the needed stuff
module Test.Most
  ( module Test.Check
  , module Test.Check.Utils
  , module Test.Operators
  , module Test.Types
  )
where

import Test.Check
import Test.Check.Utils
import Test.Operators
import Test.Types
