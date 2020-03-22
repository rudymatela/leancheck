-- |
-- Module      : Test.LeanCheck.Function.Eq
-- Copyright   : (c) 2015-2020 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- A toy Eq instance for Functions.
--
-- Warning: this is only intended to be used in testing modules.  Avoid
-- importing this on modules that are used as libraries.
module Test.LeanCheck.Function.Eq () where

import Test.LeanCheck.Core

instance (Listable a, Eq b) => Eq (a -> b) where
  f == g  =  and [f x == g x | x <- take 60 list]
