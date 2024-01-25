-- |
-- Module      : Test.LeanCheck.Function.Listable
-- Copyright   : (c) 2015-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports a 'Test.LeanCheck.Listable' instance for functions.
--
-- LeanCheck provides one definition of 'Test.LeanCheck.Listable' functions:
--
-- * "Test.LeanCheck.Function.Listable.ListsOfPairs":
--   considers functions as a finite list of exceptional input-output cases to
--   a default value (list of pairs of arguments and results).
--   This is the LeanCheck default, and is the one exported by this module.
--
-- In the future, alternative instances could be provided in sub-modules.
--
-- Warning: this is only intended to be used in testing modules.  Avoid
-- importing this on modules that are used as libraries.
module Test.LeanCheck.Function.Listable () where

import Test.LeanCheck.Function.Listable.ListsOfPairs ()
