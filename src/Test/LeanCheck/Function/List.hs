-- |
-- Module      : Test.LeanCheck.Function.List
-- Copyright   : (c) 2015-2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports functions to convert functions to lists of return
-- values and producing comparisons between functions.
module Test.LeanCheck.Function.List
  ( funToList
  , funToListMaybe
  , funToListEither
  , areEqualFor
  , compareFor
  )
where


import Test.LeanCheck
import Test.LeanCheck.Error (errorToNothing, errorToLeft)
import Data.Function (on)


-- | Converts a function to a list of result values
--   for each 'Listable' argument value.
--
-- > > list  ::  [Bool]
-- > [False,True]
-- > > funToList not
-- > [True,False]
--
-- > > list  ::  [(Bool,Bool)]
-- > [(False,False),(False,True),(True,False),(True,True)]
-- > > funToList $ uncurry (&&)
-- > [False,False,False,True]
--
-- This function may return an infinite list,
-- use 'take' as required.
--
-- > > take 10 $ list  ::  [Int]
-- > [0,1,-1,2,-2,3,-3,4,-4,5]
-- > > take 10 $ funToList (+1)  ::  [Int]
-- > [1,2,0,3,-1,4,-2,5,-3,6]
funToList :: Listable a => (a -> b) -> [b]
funToList f  =  map f list


-- | Converts a function to a list of 'Just' result values
--   or 'Nothing' on error.
--
-- > > take 6 $ funToListMaybe $ head :: [Maybe Int]
-- > [Nothing,Just 0,Just 0,Just 1,Just 0,Just 0]
--
-- This uses 'errorToNothing' and consequently 'unsafePerformIO'.
funToListMaybe :: Listable a => (a -> b) -> [Maybe b]
funToListMaybe f  =  map (errorToNothing . f) list


-- | Converts a function to a list of 'Just' result values
--   or 'Nothing' on error.
--
-- > > take 6 $ funToListEither $ head :: [Either String Int]
-- > [Left "Prelude.head: empty list",Right 0,Right 0,Right 1,Right 0,Right 0]
--
-- This uses 'errorToLeft' and consequently 'unsafePerformIO'.
funToListEither :: Listable a => (a -> b) -> [Either String b]
funToListEither f  =  map (errorToLeft . f) list


-- | This function can be used to define an Eq instance for functions based on
--   testing and equality of returned values, like so:
--
-- > instance (Listable a, Eq b) => Eq (a -> b) where
-- >   (==)  =  areEqualFor 12
--
-- This catches errors and undefined values and treats them as equal.
areEqualFor :: (Listable a, Eq b) => Int -> (a -> b) -> (a -> b) -> Bool
areEqualFor n  =  (==) `on` (take n . funToListMaybe)


-- | This function can be used to define an Ord instance for functions based on
--   testing and ordering of returned values, like so:
--
-- > instance (Listable a, Ord b) => Ord (a -> b) where
-- >   compare  =  compareFor 12
--
-- This catches errors and undefined values and treats them as equal.
compareFor :: (Listable a, Ord b) => Int -> (a -> b) -> (a -> b) -> Ordering
compareFor n  =  compare `on` (take n . funToListMaybe)
