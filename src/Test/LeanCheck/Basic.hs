-- |
-- Module      : Test.LeanCheck.Basic
-- Copyright   : (c) 2015-2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports "Test.LeanCheck.Core" along with:
--
--   * support for 'Listable' 6-tuples up to 12-tuples;
--   * 'tiers' constructors (@consN@) with arities from 6 up to 12;
--   * a 'Listable' 'Ratio' instance (consequently 'Listable' 'Rational');
--   * a 'Listable' 'Word' instance;
--   * the operators 'addWeight' and 'ofWeight'.
--
-- "Test.LeanCheck" already exports everything from this module.
-- You are probably better off importing it.
--
-- You should /only/ import "Test.LeanCheck.Basic"
-- if you /only/ want the above basic functionality.
module Test.LeanCheck.Basic
  ( module Test.LeanCheck.Core

  , cons6
  , cons7
  , cons8
  , cons9
  , cons10
  , cons11
  , cons12

  , ofWeight
  , addWeight
  )
where

-- TODO: Listable Int8/16/32/64, Word8/16/32/64, Natural

import Test.LeanCheck.Core
import Data.Word (Word)
import Data.Ratio

instance (Listable a, Listable b, Listable c,
          Listable d, Listable e, Listable f) =>
         Listable (a,b,c,d,e,f) where
  tiers = productWith (\x (y,z,w,v,u) -> (x,y,z,w,v,u)) tiers tiers

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g) =>
         Listable (a,b,c,d,e,f,g) where
  tiers = productWith (\x (y,z,w,v,u,r) -> (x,y,z,w,v,u,r)) tiers tiers

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h) =>
         Listable (a,b,c,d,e,f,g,h) where
  tiers = productWith (\x (y,z,w,v,u,r,s) -> (x,y,z,w,v,u,r,s))
                      tiers tiers

instance (Listable a, Listable b, Listable c, Listable d, Listable e,
          Listable f, Listable g, Listable h, Listable i) =>
         Listable (a,b,c,d,e,f,g,h,i) where
  tiers = productWith (\x (y,z,w,v,u,r,s,t) -> (x,y,z,w,v,u,r,s,t))
                      tiers tiers

instance (Listable a, Listable b, Listable c, Listable d, Listable e,
          Listable f, Listable g, Listable h, Listable i, Listable j) =>
         Listable (a,b,c,d,e,f,g,h,i,j) where
  tiers = productWith (\x (y,z,w,v,u,r,s,t,o) -> (x,y,z,w,v,u,r,s,t,o))
                      tiers tiers

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h,
          Listable i, Listable j, Listable k) =>
         Listable (a,b,c,d,e,f,g,h,i,j,k) where
  tiers = productWith (\x (y,z,w,v,u,r,s,t,o,p) -> (x,y,z,w,v,u,r,s,t,o,p))
                      tiers tiers

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h,
          Listable i, Listable j, Listable k, Listable l) =>
         Listable (a,b,c,d,e,f,g,h,i,j,k,l) where
  tiers = productWith (\x (y,z,w,v,u,r,s,t,o,p,q) ->
                        (x,y,z,w,v,u,r,s,t,o,p,q))
                      tiers tiers

cons6 :: (Listable a, Listable b, Listable c, Listable d, Listable e, Listable f)
      => (a -> b -> c -> d -> e -> f -> g) -> [[g]]
cons6 f = delay $ mapT (uncurry6 f) tiers

cons7 :: (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g)
      => (a -> b -> c -> d -> e -> f -> g -> h) -> [[h]]
cons7 f = delay $ mapT (uncurry7 f) tiers

cons8 :: (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h)
      => (a -> b -> c -> d -> e -> f -> g -> h -> i) -> [[i]]
cons8 f = delay $ mapT (uncurry8 f) tiers

cons9 :: (Listable a, Listable b, Listable c, Listable d, Listable e,
          Listable f, Listable g, Listable h, Listable i)
      => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> [[j]]
cons9 f = delay $ mapT (uncurry9 f) tiers

cons10 :: (Listable a, Listable b, Listable c, Listable d, Listable e,
           Listable f, Listable g, Listable h, Listable i, Listable j)
       => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> [[k]]
cons10 f = delay $ mapT (uncurry10 f) tiers

cons11 :: (Listable a, Listable b, Listable c, Listable d,
           Listable e, Listable f, Listable g, Listable h,
           Listable i, Listable j, Listable k)
       => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l) -> [[l]]
cons11 f = delay $ mapT (uncurry11 f) tiers

cons12 :: (Listable a, Listable b, Listable c, Listable d,
           Listable e, Listable f, Listable g, Listable h,
           Listable i, Listable j, Listable k, Listable l)
       => (a->b->c->d->e->f->g->h->i->j->k->l->m) -> [[m]]
cons12 f = delay $ mapT (uncurry12 f) tiers

uncurry6 :: (a->b->c->d->e->f->g) -> (a,b,c,d,e,f) -> g
uncurry6 f (x,y,z,w,v,u) = f x y z w v u

uncurry7 :: (a->b->c->d->e->f->g->h) -> (a,b,c,d,e,f,g) -> h
uncurry7 f (x,y,z,w,v,u,r) = f x y z w v u r

uncurry8 :: (a->b->c->d->e->f->g->h->i) -> (a,b,c,d,e,f,g,h) -> i
uncurry8 f (x,y,z,w,v,u,r,s) = f x y z w v u r s

uncurry9 :: (a->b->c->d->e->f->g->h->i->j) -> (a,b,c,d,e,f,g,h,i) -> j
uncurry9 f (x,y,z,w,v,u,r,s,t) = f x y z w v u r s t

uncurry10 :: (a->b->c->d->e->f->g->h->i->j->k) -> (a,b,c,d,e,f,g,h,i,j) -> k
uncurry10 f (x,y,z,w,v,u,r,s,t,o) = f x y z w v u r s t o

uncurry11 :: (a->b->c->d->e->f->g->h->i->j->k->l)
          -> (a,b,c,d,e,f,g,h,i,j,k) -> l
uncurry11 f (x,y,z,w,v,u,r,s,t,o,p) = f x y z w v u r s t o p

uncurry12 :: (a->b->c->d->e->f->g->h->i->j->k->l->m)
          -> (a,b,c,d,e,f,g,h,i,j,k,l) -> m
uncurry12 f (x,y,z,w,v,u,r,s,t,o,p,q) = f x y z w v u r s t o p q

-- | > list :: [Rational] =
--   >   [   0  % 1
--   >   ,   1  % 1
--   >   , (-1) % 1
--   >   ,   1  % 2,   2  % 1
--   >   , (-1) % 2, (-2) % 1
--   >   ,   1  % 3,   3  % 1
--   >   , (-1) % 3, (-3) % 1
--   >   ,   1  % 4,   2  % 3,   3  % 2,   4  % 1
--   >   , (-1) % 4, (-2) % 3, (-3) % 2, (-4) % 1
--   >   ,   1  % 5,   5  % 1
--   >   , (-1) % 5, (-5) % 1
--   >   , ...
--   >   ]
instance (Integral a, Listable a) => Listable (Ratio a) where
  tiers = mapT (uncurry (%)) . reset
        $ tiers `suchThat` (\(n,d) -> d > 0 && n `gcd` d == 1)

-- | > list :: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...]
instance Listable Word where
  list = [0..]

-- | Resets the weight of a constructor (or tiers)
-- Typically used as an infix constructor when defining Listable instances:
--
-- > cons<N> `ofWeight` <W>
--
-- Be careful: do not apply @`ofWeight` 0@ to recursive data structure
-- constructors.  In general this will make the list of size 0 infinite,
-- breaking the tier invariant (each tier must be finite).
--
-- 'ofWeight' is closely related to 'reset'.
ofWeight :: [[a]] -> Int -> [[a]]
ofWeight xss w = dropWhile null xss `addWeight` w

-- | Adds to the weight of tiers of a constructor
--
-- 'addWeight' is closely related to 'delay'.
addWeight :: [[a]] -> Int -> [[a]]
addWeight xss w = replicate w [] ++ xss
