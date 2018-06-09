-- Copyright 2018 Rudy Matela
--
-- Backport of Data.Function for Hugs 2006-09.
-- Only exports `on`
module Data.Function
  ( on
  )
where

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x y -> f x .*. f y
infixl 0 `on`
