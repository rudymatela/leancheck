-- | Simple property-based testing library based on
--   enumeration of values via lists of lists.
--
-- This module exports "Test.LeanCheck.Core" functionality along with instances and
-- functions for further tuple and constructor arities.
--
-- For the complete list of functions, see "Test.LeanCheck".
module Test.LeanCheck.Basic
  ( module Test.LeanCheck.Core

  , cons6
  , cons7
  , cons8
  , cons9
  , cons10
  , cons11
  , cons12
  )
where

import Test.LeanCheck.Core

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
cons6 f = mapT (uncurry6 f) tiers `addWeight` 1

cons7 :: (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g)
      => (a -> b -> c -> d -> e -> f -> g -> h) -> [[h]]
cons7 f = mapT (uncurry7 f) tiers `addWeight` 1

cons8 :: (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h)
      => (a -> b -> c -> d -> e -> f -> g -> h -> i) -> [[i]]
cons8 f = mapT (uncurry8 f) tiers `addWeight` 1

cons9 :: (Listable a, Listable b, Listable c, Listable d, Listable e,
          Listable f, Listable g, Listable h, Listable i)
      => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> [[j]]
cons9 f = mapT (uncurry9 f) tiers `addWeight` 1

cons10 :: (Listable a, Listable b, Listable c, Listable d, Listable e,
           Listable f, Listable g, Listable h, Listable i, Listable j)
       => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> [[k]]
cons10 f = mapT (uncurry10 f) tiers `addWeight` 1

cons11 :: (Listable a, Listable b, Listable c, Listable d,
           Listable e, Listable f, Listable g, Listable h,
           Listable i, Listable j, Listable k)
       => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l) -> [[l]]
cons11 f = mapT (uncurry11 f) tiers `addWeight` 1

cons12 :: (Listable a, Listable b, Listable c, Listable d,
           Listable e, Listable f, Listable g, Listable h,
           Listable i, Listable j, Listable k, Listable l)
       => (a->b->c->d->e->f->g->h->i->j->k->l->m) -> [[m]]
cons12 f = mapT (uncurry12 f) tiers `addWeight` 1

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
