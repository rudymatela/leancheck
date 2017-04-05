-- |
-- Module      : Test.LeanCheck.CoListable
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports a 'Listable' instance for function enumeration by means
-- of a 'CoListable' typeclass.
--
-- This module /does not currently work/, it it just a sketch and a stub.
module Test.LeanCheck.Function.CoListable
where


import Test.LeanCheck
import Test.LeanCheck.Tiers
import Data.Maybe (fromMaybe)


instance (CoListable a, Listable b) => Listable (a -> b) where
  tiers = cotiers tiers


(\+:/) :: [[a]] -> [[a]] -> [[a]]
xss \+:/ yss = xss \/ ([]:yss)
infixr 9 \+:/


class CoListable a where
  cotiers :: [[b]] -> [[a -> b]]


instance CoListable () where
  cotiers rs = mapT (\r  () -> r) rs


instance CoListable Bool where
  cotiers rs = productWith (\r1 r2  b -> if b then r1 else r2) rs rs


instance CoListable a => CoListable (Maybe a) where
  cotiers rs = productWith (\z f  m -> case m of
                                         Nothing -> z
                                         Just x  -> f x) rs (cotiers rs)


instance (CoListable a, CoListable b) => CoListable (Either a b) where
  cotiers rs = productWith (\f g  e -> case e of
                                         Left x  -> f x
                                         Right x -> g x) (cotiers rs) (cotiers rs)


instance (CoListable a) => CoListable [a] where
  cotiers rss = mapT const rss
           \+:/ productWith
                  (\y f  xs -> case xs of
                                 []      -> y
                                 (x:xs') -> f x xs')
                  rss
                  (cotiers (cotiers rss))


instance (CoListable a, CoListable b) => CoListable (a,b) where
  cotiers = mapT uncurry . cotiers . cotiers


instance CoListable Int where
  cotiers rss = mapT const rss
           \+:/ productWith
                  (\f g  i -> if i >= 0 then f (i-1) else g (i+1))
                  (cotiers rss) (cotiers rss)


alts0 :: [[a]] -> [[a]]
alts0 = id

alts1 :: CoListable a => [[b]] -> [[a->b]]
alts1 bs = cotiers bs

alts2 :: (CoListable a, CoListable b) => [[c]] -> [[a->b->c]]
alts2 cs = cotiers (cotiers cs)

alts3 :: (CoListable a, CoListable b, CoListable c) => [[d]] -> [[a->b->c->d]]
alts3 ds = cotiers (cotiers (cotiers ds))

ftiers :: (CoListable a, Listable b) => [[a->b]]
ftiers = cotiers tiers
