-- |
-- Module      : Test.LeanCheck.Function.Ord
-- Copyright   : (c) 2015-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This /optional/ module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- Toy 'Eq' and 'Ord' instance for functions.
--
-- > instance (Listable a, Eq b) => Eq (a -> b) where
-- >   (==)  =  areEqualFor 12
--
-- > instance (Listable a, Ord b) => Ord (a -> b) where
-- >   compare  =  compareFor 12
--
-- This compares functions by testing them
-- for up to 12 different values of each argument.
--
-- Single argument functions are tested 12 times.
-- Two argument functions are tested 144 times.
-- Three argument functions are tested 1728 times.
-- At each subsequent argument,
-- number of tests and runtime increases 12-fold.
--
-- To customize the number of tests,
-- don't import this and use the above code changing the @12@ value.
-- Keep in mind that this value is number of tests for /each/ argument.
--
-- __Warning:__
-- this is only intended to be used in testing modules.
-- Avoid importing this on modules that are used as libraries
-- as there is no way to unimport a typeclass instance.
module Test.LeanCheck.Function.Ord () where

import Test.LeanCheck.Core
import Test.LeanCheck.Function.List
import Test.LeanCheck.Function.Eq ()

-- | Two functions are compared based on the results
--   of twelve different enumerated values for each argument.
--
-- Single argument functions are tested 12 times.
-- Two argument functions are tested 144 times.
-- Three argument functions are tested 1728 times.
-- At each subsequent argument,
-- number of tests and runtime increases 12-fold.
--
-- This is only intended to be available on test programs and modules
-- but not on libraries.  If you see this exported on a library
-- that is not specifically intended to consider equality between
-- functions, file a bug report to the library maintainer.
instance (Listable a, Ord b) => Ord (a -> b) where
  compare  =  compareFor 12
