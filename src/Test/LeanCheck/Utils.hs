-- |
-- Module      : Test.LeanCheck.Utils
-- Copyright   : (c) 2015-2020 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- Some utilities for property-based testing with 'Test.LeanCheck'.
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
  , module Test.LeanCheck.Stats
  )
where

import Test.LeanCheck.Utils.Types
import Test.LeanCheck.Utils.Operators
import Test.LeanCheck.Utils.TypeBinding
import Test.LeanCheck.Stats
