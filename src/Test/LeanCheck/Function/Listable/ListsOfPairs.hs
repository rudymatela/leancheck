-- |
-- Module      : Test.LeanCheck.Function.ListsOfPairs
-- Copyright   : (c) 2015-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports a 'Listable' instance for function enumeration
-- via lists of pairs.
--
-- This module considers functions as a finite list of exceptional input-output
-- cases to a default value (list of pairs of arguments and results).
module Test.LeanCheck.Function.Listable.ListsOfPairs () where

import Test.LeanCheck.Core
import Test.LeanCheck.Function.ListsOfPairs

instance (Eq a, Listable a, Listable b) => Listable (a -> b) where
  tiers  =  tiers -->> tiers
