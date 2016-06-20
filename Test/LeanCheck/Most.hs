-- | Simple property-based testing library
--   based on enumeration of values via lists of lists.
--
-- This module exports Most modules that accompain Test.Check
-- and is to be used as a shorthand:
--
-- > import Test.LeanCheck.Most
--
-- To get most of the needed stuff
module Test.LeanCheck.Most
  ( module Test.LeanCheck
  , module Test.LeanCheck.Operators
  , module Test.LeanCheck.TypeBinding
  , module Test.LeanCheck.Types
  )
where

import Test.LeanCheck
import Test.LeanCheck.Operators
import Test.LeanCheck.TypeBinding
import Test.LeanCheck.Types
