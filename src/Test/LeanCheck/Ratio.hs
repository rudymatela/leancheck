-- | LeanCheck is a simple enumerative property-based testing library.
--
-- This module exports a 'Listable' 'Ratio' instance
-- (consequently 'Listable' 'Rational').
-- This instance is also in scope when you import:
--
--   * "Test.LeanCheck";
--   * "Test.LeanCheck.Basic";
--   * and any modules that export the two above.
--
-- So you better be importing the above modules instead of this.
-- I decided not to export this from "Test.LeanCheck.Core" to keep it "lean".
--
-- Don't worry if you don't use 'Rational's,
-- /only/ the typeclass instance is exported --- not "Data.Ratio".
module Test.LeanCheck.Ratio () where

import Test.LeanCheck.Core
import Data.Ratio

instance (Integral a, Listable a) => Listable (Ratio a) where
  tiers = mapT (uncurry (%))
        $ tiers `suchThat` (\(n,d) -> d > 0 && n `gcd` d == 1)
                `ofWeight` 0 -- make size 0 not be "usually" empty
