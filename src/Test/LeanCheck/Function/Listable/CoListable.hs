-- |
-- Module      : Test.LeanCheck.Function.Listable.CoListable
-- Copyright   : (c) 2015-2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports a 'Listable' instance for function enumeration by means
-- of a 'CoListable' typeclass.  This is very similar to the coseries
-- enumeration of SmallCheck.
--
-- This module /does not currently work/, it it just a sketch and a stub.
module Test.LeanCheck.Function.Listable.CoListable () where

import Test.LeanCheck.Core
import Test.LeanCheck.Function.CoListable

instance (CoListable a, Listable b) => Listable (a -> b) where
  tiers = cotiers tiers
