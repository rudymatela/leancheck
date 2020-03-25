{-# LANGUAGE CPP #-}
-- |
-- Module      : Test.LeanCheck.Basic
-- Copyright   : (c) 2015-2020 Rudy Matela
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
--   * a 'Listable' 'Word' instance;
--   * a 'Listable' 'Ratio' instance (consequently 'Listable' 'Rational');
--   * a 'Listable' 'Complex' instance;
--   * 'Listable' 'Int8/16/32/64' instances;
--   * 'Listable' 'Word8/16/32/64' instances;
--   * 'Listable' instances for "Foreign.C" types;
--   * a 'Listable' 'ExitCode' instance;
--   * a 'Listable' 'GeneralCategory' instance;
--   * 'Listable' 'Buffer/IO/SeekMode' instances;
--   * the operators 'addWeight' and 'ofWeight'.
--
-- The above includes all types defined in the Haskell 2010 Report
-- with the exception of Array, IO, Handle, HandlePosn, IOErrorType.
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

import Test.LeanCheck.Core
import Data.Ratio
import Data.Complex
import Data.Int
import Data.Word
import Data.Char (GeneralCategory)
import System.IO (IOMode (..), BufferMode (..), SeekMode (..))
import Foreign.C
import System.Exit

instance (Listable a, Listable b, Listable c,
          Listable d, Listable e, Listable f) =>
         Listable (a,b,c,d,e,f) where
  tiers  =  productWith (\x (y,z,w,v,u) -> (x,y,z,w,v,u)) tiers tiers

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g) =>
         Listable (a,b,c,d,e,f,g) where
  tiers  =  productWith (\x (y,z,w,v,u,r) -> (x,y,z,w,v,u,r)) tiers tiers

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h) =>
         Listable (a,b,c,d,e,f,g,h) where
  tiers  =  productWith (\x (y,z,w,v,u,r,s) -> (x,y,z,w,v,u,r,s)) tiers tiers

instance (Listable a, Listable b, Listable c, Listable d, Listable e,
          Listable f, Listable g, Listable h, Listable i) =>
         Listable (a,b,c,d,e,f,g,h,i) where
  tiers  =  productWith (\x (y,z,w,v,u,r,s,t) -> (x,y,z,w,v,u,r,s,t))
                        tiers tiers

instance (Listable a, Listable b, Listable c, Listable d, Listable e,
          Listable f, Listable g, Listable h, Listable i, Listable j) =>
         Listable (a,b,c,d,e,f,g,h,i,j) where
  tiers  =  productWith (\x (y,z,w,v,u,r,s,t,o) -> (x,y,z,w,v,u,r,s,t,o))
                        tiers tiers

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h,
          Listable i, Listable j, Listable k) =>
         Listable (a,b,c,d,e,f,g,h,i,j,k) where
  tiers  =  productWith (\x (y,z,w,v,u,r,s,t,o,p) -> (x,y,z,w,v,u,r,s,t,o,p))
                        tiers tiers

instance (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h,
          Listable i, Listable j, Listable k, Listable l) =>
         Listable (a,b,c,d,e,f,g,h,i,j,k,l) where
  tiers  =  productWith (\x (y,z,w,v,u,r,s,t,o,p,q) ->
                          (x,y,z,w,v,u,r,s,t,o,p,q))
                        tiers tiers

-- | Returns tiers of applications of a 6-argument constructor.
cons6 :: (Listable a, Listable b, Listable c, Listable d, Listable e, Listable f)
      => (a -> b -> c -> d -> e -> f -> g) -> [[g]]
cons6 f  =  delay $ mapT (uncurry6 f) tiers

-- | Returns tiers of applications of a 7-argument constructor.
cons7 :: (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g)
      => (a -> b -> c -> d -> e -> f -> g -> h) -> [[h]]
cons7 f  =  delay $ mapT (uncurry7 f) tiers

-- | Returns tiers of applications of a 8-argument constructor.
cons8 :: (Listable a, Listable b, Listable c, Listable d,
          Listable e, Listable f, Listable g, Listable h)
      => (a -> b -> c -> d -> e -> f -> g -> h -> i) -> [[i]]
cons8 f  =  delay $ mapT (uncurry8 f) tiers

-- | Returns tiers of applications of a 9-argument constructor.
cons9 :: (Listable a, Listable b, Listable c, Listable d, Listable e,
          Listable f, Listable g, Listable h, Listable i)
      => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j) -> [[j]]
cons9 f  =  delay $ mapT (uncurry9 f) tiers

-- | Returns tiers of applications of a 10-argument constructor.
cons10 :: (Listable a, Listable b, Listable c, Listable d, Listable e,
           Listable f, Listable g, Listable h, Listable i, Listable j)
       => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) -> [[k]]
cons10 f  =  delay $ mapT (uncurry10 f) tiers

-- | Returns tiers of applications of a 11-argument constructor.
cons11 :: (Listable a, Listable b, Listable c, Listable d,
           Listable e, Listable f, Listable g, Listable h,
           Listable i, Listable j, Listable k)
       => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l) -> [[l]]
cons11 f  =  delay $ mapT (uncurry11 f) tiers

-- | Returns tiers of applications of a 12-argument constructor.
cons12 :: (Listable a, Listable b, Listable c, Listable d,
           Listable e, Listable f, Listable g, Listable h,
           Listable i, Listable j, Listable k, Listable l)
       => (a->b->c->d->e->f->g->h->i->j->k->l->m) -> [[m]]
cons12 f  =  delay $ mapT (uncurry12 f) tiers

uncurry6 :: (a->b->c->d->e->f->g) -> (a,b,c,d,e,f) -> g
uncurry6 f (x,y,z,w,v,u)  =  f x y z w v u

uncurry7 :: (a->b->c->d->e->f->g->h) -> (a,b,c,d,e,f,g) -> h
uncurry7 f (x,y,z,w,v,u,r)  =  f x y z w v u r

uncurry8 :: (a->b->c->d->e->f->g->h->i) -> (a,b,c,d,e,f,g,h) -> i
uncurry8 f (x,y,z,w,v,u,r,s)  =  f x y z w v u r s

uncurry9 :: (a->b->c->d->e->f->g->h->i->j) -> (a,b,c,d,e,f,g,h,i) -> j
uncurry9 f (x,y,z,w,v,u,r,s,t)  =  f x y z w v u r s t

uncurry10 :: (a->b->c->d->e->f->g->h->i->j->k) -> (a,b,c,d,e,f,g,h,i,j) -> k
uncurry10 f (x,y,z,w,v,u,r,s,t,o)  =  f x y z w v u r s t o

uncurry11 :: (a->b->c->d->e->f->g->h->i->j->k->l)
          -> (a,b,c,d,e,f,g,h,i,j,k) -> l
uncurry11 f (x,y,z,w,v,u,r,s,t,o,p)  =  f x y z w v u r s t o p

uncurry12 :: (a->b->c->d->e->f->g->h->i->j->k->l->m)
          -> (a,b,c,d,e,f,g,h,i,j,k,l) -> m
uncurry12 f (x,y,z,w,v,u,r,s,t,o,p,q)  =  f x y z w v u r s t o p q

-- | > list :: [Rational]  =
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
  tiers  =  mapT (uncurry (%)) . reset
        $ tiers `suchThat` (\(n,d) -> d > 0 && n `gcd` d == 1)

instance (RealFloat a, Listable a) => Listable (Complex a) where
  tiers  =  cons2 (:+)

-- | > list :: [Word]  =  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...]
instance Listable Word where
  list  =  listIntegral

-- | > list :: [Word8]  =  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, ..., 255]
instance Listable Word8 where
  list  =  listIntegral

-- | > list :: [Word16]  =  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, ..., 65535]
instance Listable Word16 where
  list  =  listIntegral

-- | > list :: [Word32]  =  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...]
instance Listable Word32 where
  list  =  listIntegral

-- | > list :: [Word64]  =  [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, ...]
instance Listable Word64 where
  list  =  listIntegral

-- | > list :: [Int8]  =  [0, 1, -1, 2, -2, 3, -3, ..., 127, -127, -128]
instance Listable Int8 where
  list  =  listIntegral

-- | > list :: [Int16]  =  [0, 1, -1, 2, -2, ..., 32767, -32767, -32768]
instance Listable Int16 where
  list  =  listIntegral

-- | > list :: [Int32]  =  [0, 1, -1, 2, -2, 3, -3, 4, -4, 5, -5, 6, ...]
instance Listable Int32 where
  list  =  listIntegral

-- | > list :: [Int64]  =  [0, 1, -1, 2, -2, 3, -3, 4, -4, 5, -5, 6, ...]
instance Listable Int64 where
  list  =  listIntegral

instance Listable CChar      where  list  =  listIntegral
instance Listable CSChar     where  list  =  listIntegral
instance Listable CUChar     where  list  =  listIntegral
instance Listable CShort     where  list  =  listIntegral
instance Listable CUShort    where  list  =  listIntegral
instance Listable CInt       where  list  =  listIntegral
instance Listable CUInt      where  list  =  listIntegral
instance Listable CLong      where  list  =  listIntegral
instance Listable CULong     where  list  =  listIntegral
instance Listable CPtrdiff   where  list  =  listIntegral
instance Listable CSize      where  list  =  listIntegral
instance Listable CWchar     where  list  =  listIntegral
instance Listable CSigAtomic where  list  =  listIntegral
instance Listable CLLong     where  list  =  listIntegral
instance Listable CULLong    where  list  =  listIntegral
instance Listable CIntPtr    where  list  =  listIntegral
instance Listable CUIntPtr   where  list  =  listIntegral
instance Listable CIntMax    where  list  =  listIntegral
instance Listable CUIntMax   where  list  =  listIntegral
instance Listable CClock     where  list  =  listIntegral
instance Listable CTime      where  list  =  listIntegral
instance Listable CFloat     where  tiers  =  tiersFloating
instance Listable CDouble    where  tiers  =  tiersFloating
#if __GLASGOW_HASKELL__ >= 802
instance Listable CBool      where  list  =  listIntegral
#endif
#if __GLASGOW_HASKELL__
instance Listable CUSeconds  where  list  =  listIntegral
instance Listable CSUSeconds where  list  =  listIntegral
#endif

-- | Only includes valid POSIX exit codes
--
-- > > list :: [ExitCode]
-- > [ExitSuccess, ExitFailure 1, ExitFailure 2, ..., ExitFailure 255]
instance Listable ExitCode where  list  =  ExitSuccess : map ExitFailure [1..255]

instance Listable GeneralCategory where  list  =  [minBound..maxBound]

instance Listable IOMode where
  tiers  =  cons0 ReadMode
         \/ cons0 WriteMode
         \/ cons0 AppendMode
         \/ cons0 ReadWriteMode

instance Listable BufferMode where
  tiers  =  cons0 NoBuffering
         \/ cons0 LineBuffering
         \/ cons1 BlockBuffering

instance Listable SeekMode where
  tiers  =  cons0 AbsoluteSeek
         \/ cons0 RelativeSeek
         \/ cons0 SeekFromEnd

-- | Resets the weight of a constructor or tiers.
--
-- > > [ [], [], ..., xs, ys, zs, ... ] `ofWeight` 1
-- > [ [], xs, ys, zs, ... ]
--
-- > > [ xs, ys, zs, ... ] `ofWeight` 2
-- > [ [], [], xs, ys, zs, ... ]
--
-- > > [ [], xs, ys, zs, ... ] `ofWeight` 3
-- > [ [], [], [], xs, ys, zs, ... ]
--
-- Typically used as an infix operator when defining 'Listable' instances:
--
-- > instance Listable <Type> where
-- >   tiers  =  ...
-- >          \/ cons<N> <Cons>  `ofWeight`  <W>
-- >          \/ ...
--
-- /Warning:/ do not apply @ \`ofWeight\` 0 @ to recursive data structure
-- constructors.  In general this will make the list of size 0 infinite,
-- breaking the tier invariant (each tier must be finite).
--
-- @ \`ofWeight\` /n/ @ is equivalent to 'reset' followed
-- by @/n/@ applications of 'delay'.
ofWeight :: [[a]] -> Int -> [[a]]
ofWeight xss w  =  dropWhile null xss `addWeight` w

-- | Adds to the weight of a constructor or tiers.
--
-- > instance Listable <Type> where
-- >   tiers  =  ...
-- >          \/ cons<N> <Cons>  `addWeight`  <W>
-- >          \/ ...
--
-- Typically used as an infix operator when defining 'Listable' instances:
--
-- > > [ xs, ys, zs, ... ] `addWeight` 1
-- > [ [], xs, ys, zs, ... ]
--
-- > > [ xs, ys, zs, ... ] `addWeight` 2
-- > [ [], [], xs, ys, zs, ... ]
--
-- > > [ [], xs, ys, zs, ... ] `addWeight` 3
-- > [ [], [], [], [], xs, ys, zs, ... ]
--
-- @ \`addWeight\` /n/ @ is equivalent to @/n/@ applications of 'delay'.
addWeight :: [[a]] -> Int -> [[a]]
addWeight xss w  =  replicate w [] ++ xss
