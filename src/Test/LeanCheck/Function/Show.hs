-- |
-- Module      : Test.LeanCheck.Function.Show
-- Copyright   : (c) 2015-2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- A 'Show' instance for functions.
--
-- Warning: this is only intended to be used in testing modules.  Avoid
-- importing this on modules that are used as libraries.
module Test.LeanCheck.Function.Show () where

import Test.LeanCheck.Function.ShowFunction

instance (Show a, Listable a, ShowFunction b) => Show (a->b) where
  showsPrec _ = (++) . shw
    where
    shw f = case name 60 f of
            Nothing -> showFunction 8 f
            Just nm -> nm
