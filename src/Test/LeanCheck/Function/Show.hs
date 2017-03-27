-- |
-- Module      : Test.LeanCheck.Function.Show
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- A 'Show' instance for functions.
module Test.LeanCheck.Function.Show () where

import Test.LeanCheck.Function.ShowFunction

instance (Show a, Listable a, ShowFunction b) => Show (a->b) where
  showsPrec 0 = (++) . showFunction 8
  showsPrec _ = (++) . paren . showFunctionLine 4
    where paren s = "(" ++ s ++ ")"

