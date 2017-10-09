-- |
-- Module      : Test.LeanCheck.Function.Mixed
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports a 'Listable' instance for function enumeration
-- by combining several enumeration strategies.
module Test.LeanCheck.Function.Listable.Mixed () where

import Test.LeanCheck.Core
import Test.LeanCheck.Function.ListsOfPairs
import Test.LeanCheck.Function.CoListable

instance (Eq a, Listable a, CoListable a, Listable b) => Listable (a -> b) where
  tiers = tiers -->> tiers
       \/ cotiers tiers
