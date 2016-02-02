-- | Simple property-based testing library based on
--   enumeration of values via lists of lists.
module Test.Check.Basic
  ( module Test.Check.Core

  , cons6
  , cons7
  , cons8
  , cons9
  , cons10
  , cons11
  , cons12

  , wcons6
  , wcons7
  , wcons8
  , wcons9
  , wcons10
  , wcons11
  , wcons12
  )
where

import Test.Check.Core hiding (zipWith')

instance (Listable a, Listable b, Listable c,
          Listable d, Listable e, Listable f) =>
         Listable (a,b,c,d,e,f) where
  listing = lsProductWith (\x (y,z,w,v,u) -> (x,y,z,w,v,u)) listing listing

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g) =>
         Listable (a,b,c,d,e,f,g) where
  listing = lsProductWith (\x (y,z,w,v,u,r) -> (x,y,z,w,v,u,r)) listing listing

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h) =>
         Listable (a,b,c,d,e,f,g,h) where
  listing = lsProductWith (\x (y,z,w,v,u,r,s) -> (x,y,z,w,v,u,r,s))
                          listing listing

instance (Listable a, Listable b, Listable c, Listable d, Listable e,
          Listable f, Listable g, Listable h, Listable i) =>
         Listable (a,b,c,d,e,f,g,h,i) where
  listing = lsProductWith (\x (y,z,w,v,u,r,s,t) -> (x,y,z,w,v,u,r,s,t))
                          listing listing

instance (Listable a, Listable b, Listable c, Listable d, Listable e,
          Listable f, Listable g, Listable h, Listable i, Listable j) =>
         Listable (a,b,c,d,e,f,g,h,i,j) where
  listing = lsProductWith (\x (y,z,w,v,u,r,s,t,o) -> (x,y,z,w,v,u,r,s,t,o))
                          listing listing

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h,
          Listable i, Listable j, Listable k) =>
         Listable (a,b,c,d,e,f,g,h,i,j,k) where
  listing = lsProductWith (\x (y,z,w,v,u,r,s,t,o,p) -> (x,y,z,w,v,u,r,s,t,o,p))
                          listing listing

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h,
          Listable i, Listable j, Listable k, Listable l) =>
         Listable (a,b,c,d,e,f,g,h,i,j,k,l) where
  listing = lsProductWith (\x (y,z,w,v,u,r,s,t,o,p,q) ->
                            (x,y,z,w,v,u,r,s,t,o,p,q))
                          listing listing

wcons6 :: (Listable a, Listable b, Listable c, Listable d, Listable e, Listable f)
       => Int -> (a -> b -> c -> d -> e -> f -> g) -> [[g]]
wcons6 w f = replicate w [] ++ lsmap (uncurry6 f) listing

wcons7 :: (Listable a, Listable b, Listable c, Listable d,
           Listable e, Listable f, Listable g)
       => Int -> (a -> b -> c -> d -> e -> f -> g -> h) -> [[h]]
wcons7 w f = replicate w [] ++ lsmap (uncurry7 f) listing

wcons8 :: (Listable a, Listable b, Listable c, Listable d,
           Listable e, Listable f, Listable g, Listable h)
       => Int -> (a -> b -> c -> d -> e -> f -> g -> h -> i) -> [[i]]
wcons8 w f = replicate w [] ++ lsmap (uncurry8 f) listing

wcons9 :: (Listable a, Listable b, Listable c, Listable d, Listable e,
           Listable f, Listable g, Listable h, Listable i)
       => Int -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> [[j]]
wcons9 w f = replicate w [] ++ lsmap (uncurry9 f) listing

wcons10 :: (Listable a, Listable b, Listable c, Listable d, Listable e,
            Listable f, Listable g, Listable h, Listable i, Listable j)
        => Int -> (a->b->c->d->e->f->g->h->i->j->k) -> [[k]]
wcons10 w f = replicate w [] ++ lsmap (uncurry10 f) listing

wcons11 :: (Listable a, Listable b, Listable c, Listable d,
            Listable e, Listable f, Listable g, Listable h,
            Listable i, Listable j, Listable k)
        => Int -> (a->b->c->d->e->f->g->h->i->j->k->l) -> [[l]]
wcons11 w f = replicate w [] ++ lsmap (uncurry11 f) listing

wcons12 :: (Listable a, Listable b, Listable c, Listable d,
            Listable e, Listable f, Listable g, Listable h,
            Listable i, Listable j, Listable k, Listable l)
        => Int -> (a->b->c->d->e->f->g->h->i->j->k->l->m) -> [[m]]
wcons12 w f = replicate w [] ++ lsmap (uncurry12 f) listing

cons6 :: (Listable a, Listable b, Listable c, Listable d, Listable e, Listable f)
      => (a -> b -> c -> d -> e -> f -> g) -> [[g]]
cons6 = wcons6 1

cons7 :: (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g)
      => (a -> b -> c -> d -> e -> f -> g -> h) -> [[h]]
cons7 = wcons7 1

cons8 :: (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h)
      => (a -> b -> c -> d -> e -> f -> g -> h -> i) -> [[i]]
cons8 = wcons8 1

cons9 :: (Listable a, Listable b, Listable c, Listable d, Listable e,
          Listable f, Listable g, Listable h, Listable i)
      => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> [[j]]
cons9 = wcons9 1

cons10 :: (Listable a, Listable b, Listable c, Listable d, Listable e,
           Listable f, Listable g, Listable h, Listable i, Listable j)
       => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> [[k]]
cons10 = wcons10 1

cons11 :: (Listable a, Listable b, Listable c, Listable d,
           Listable e, Listable f, Listable g, Listable h,
           Listable i, Listable j, Listable k)
       => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l) -> [[l]]
cons11 = wcons11 1

cons12 :: (Listable a, Listable b, Listable c, Listable d,
           Listable e, Listable f, Listable g, Listable h,
           Listable i, Listable j, Listable k, Listable l)
       => (a->b->c->d->e->f->g->h->i->j->k->l->m) -> [[m]]
cons12 = wcons12 1

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
