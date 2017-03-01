-- A toy Eq instance for Functions.
module Test.LeanCheck.Function.Eq () where

import Test.LeanCheck.Core

instance (Listable a, Eq b) => Eq (a -> b) where
  f == g  =  and [f x == g x | x <- take 60 list]
