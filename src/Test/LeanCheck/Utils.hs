-- | Some utilities for property-based testing with 'Test.LeanCheck'.
--
-- Those utilities are general-purpose enough to be used with other
-- property-based based testing libraries.  See each exported module for
-- details.
--
-- This is not exported by "Test.LeanCheck".  You need to import this
-- explicitly.
module Test.LeanCheck.Utils
  ( module Test.LeanCheck.Utils.Types
  , module Test.LeanCheck.Utils.Operators
  , module Test.LeanCheck.Utils.TypeBinding
  )
where

import Test.LeanCheck.Utils.Types
import Test.LeanCheck.Utils.Operators
import Test.LeanCheck.Utils.TypeBinding
