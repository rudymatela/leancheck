{-# LANGUAGE FlexibleContexts, TypeOperators #-}
-- |
-- Module      : Test.LeanCheck.Generic
-- Copyright   : (c) 2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This is an experimental module for deriving 'Listable' instances through
-- GHC's generic.
module Test.LeanCheck.Generic
  ( genericList
  , genericTiers
  )
where

import GHC.Generics
import Test.LeanCheck.Core

-- | A generic implementation of 'list' for instances of 'Generic'.
--
-- Use it to define your 'Listable' instances like so:
--
-- > instance Listable MyType where
-- >   list = genericList
--
-- Consider using 'genericTiers' instead of this
-- (unless you know what you're doing).
genericList :: (Generic a, Listable' (Rep a)) => [a]
genericList = concat genericTiers

-- | A generic implementation of 'tiers' for instances of 'Generic'.
--
-- Use it to define your 'Listable' instances like so:
--
-- > instance Listable MyType where
-- >   tiers = genericTiers
genericTiers :: (Generic a, Listable' (Rep a)) => [[a]]
genericTiers = mapT to tiers'

class Listable' f where
  tiers' :: [[f p]]

instance Listable' V1 where
  tiers' = undefined

instance Listable' U1 where
  tiers' = [[U1]]

instance Listable c => Listable' (K1 i c) where
  tiers' = mapT K1 tiers

instance (Listable' a, Listable' b) => Listable' (a :+: b) where
  tiers' = mapT L1 tiers' \/ mapT R1 tiers'

instance (Listable' a, Listable' b) => Listable' (a :*: b) where
  tiers' = productWith (:*:) tiers' tiers'

instance Listable' f => Listable' (M1 i c f) where
  tiers' = mapT M1 tiers'
