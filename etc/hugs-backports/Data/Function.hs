-- Backport of Data.Function for Hugs 2006-09.
-- Only exports `on`
--
-- Copyright (c) 2018-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
module Data.Function
  ( on
  )
where

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(?) `on` f  =  \x y -> f x ? f y
infixl 0 `on`
