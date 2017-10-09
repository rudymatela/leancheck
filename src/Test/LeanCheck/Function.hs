-- |
-- Module      : Test.LeanCheck.Function
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports 'Listable' and 'Show' function typeclass instances.
-- These can be useful for testing higher-order properties --- properties that
-- take functions as arguments.
--
-- Warning: this is only intended to be used in testing modules.  Avoid
-- importing this on modules that are used as libraries.
module Test.LeanCheck.Function () where
import Test.LeanCheck.Function.Listable ()
import Test.LeanCheck.Function.Show ()
