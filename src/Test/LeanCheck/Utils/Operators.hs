-- |
-- Module      : Test.LeanCheck.Utils.Operators
-- Copyright   : (c) 2015-2020 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- Some operators for property-based testing.
module Test.LeanCheck.Utils.Operators
  (

  -- * Combining properties
    (==>)
  , (===), (====)
  , (&&&), (&&&&), (&&&&&)
  , (|||), (||||)

  -- * Properties of unary functions
  , idempotent
  , identity
  , neverIdentity

  -- * Properties of operators (binary functions)
  , isCommutative
  , commutative
  , isAssociative
  , associative
  , isDistributiveOver
  , isLeftDistributiveOver
  , isRightDistributiveOver
  , distributive
  , symmetric2

  -- * Properties of relations (binary functions returning truth values)
  , isTransitive
  , transitive
  , isReflexive
  , reflexive
  , isIrreflexive
  , irreflexive
  , isSymmetric
  , symmetric
  , isAsymmetric
  , asymmetric
  , isAntisymmetric
  , antisymmetric

  -- ** Order relations
  , isEquivalence
  , equivalence
  , isPartialOrder
  , partialOrder
  , isStrictPartialOrder
  , strictPartialOrder
  , isTotalOrder
  , totalOrder
  , strictTotalOrder
  , comparison

  -- * Ternary comparison operators
  , (=$), ($=)
  , (=|), (|=)

  -- * Properties for typeclass instances
  , okEq
  , okOrd
  , okEqOrd
  , okNum
  , okNumNonNegative
  )
where

-- TODO: Add examples in the haddock documentation of most functions in this
--       module.

-- TODO: review terminology in this module.  Some names aren't quite right!

-- TODO: rename commutative to isCommutative and etc...

import Test.LeanCheck ((==>))

combine :: (b -> c -> d) -> (a -> b) -> (a -> c) -> (a -> d)
combine (?) f g  =  \x -> f x ? g x

-- Uneeded, just food for thought:
-- > combine2 :: (c -> d -> e) -> (a -> b -> c) -> (a -> b -> d) -> (a -> b -> e)
-- Two possible implementations:
-- > combine2 op f g = \x y -> f x y `op` g x y
-- > combine2 = combine . combine

-- | Allows building equality properties between functions.
--
-- > prop_id_idempotent  =  id === id . id
--
-- > > check $ id === (+0)
-- > +++ OK, passed 200 tests.
--
-- > > check $ id === id . id
-- > +++ OK, passed 1 tests (exhausted).
--
-- > > check $ id === (+1)
-- > *** Failed! Falsifiable (after 1 tests):
-- > 0
(===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(===)  =  combine (==)
infix 4 ===

-- | Allows building equality properties between two-argument functions.
--
-- > > holds 100 $ const ==== asTypeOf
-- > True
--
-- > > holds 100 $ (+) ==== flip (+)
-- > True
--
-- > > holds 100 $ (+) ==== (*)
-- > False
(====) :: Eq c => (a -> b -> c) -> (a -> b -> c) -> a -> b -> Bool
(====)  =  combine (===)
infix 4 ====

-- | And ('&&') operator over one-argument properties.
--
-- Allows building conjuntions between one-argument properties:
--
-- > > holds 100 $ id === (+0) &&& id === (id . id)
-- > True
(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&)  =  combine (&&)
infixr 3 &&&

-- | And ('&&') operator over two-argument properties.
--
-- Allows building conjuntions between two-argument properties:
--
-- > > holds 100 $ (+) ==== flip (+) &&&& (+) ==== (*)
-- > False
(&&&&) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
(&&&&)  =  combine (&&&)
infixr 3 &&&&

-- | And operator over three-argument properties.
(&&&&&) :: (a -> b -> c -> Bool) -> (a -> b -> c -> Bool) -> a -> b -> c -> Bool
(&&&&&)  =  combine (&&&&)
infixr 3 &&&&&

-- | Or ('||') operator over one-argument properties.
--
-- Allows building disjunctions between one-argument properties:
--
-- > > holds 100 $ id === (+0) ||| id === (id . id)
-- > True
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||)  =  combine (||)
infixr 2 |||

-- | Or ('||') operator over two-argument properties.
--
-- Allows building conjuntions between two-argument properties:
--
-- > > holds 100 $ (+) ==== flip (+) |||| (+) ==== (*)
-- > True
(||||) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
(||||)  =  combine (|||)
infixr 2 ||||

-- | Is a given operator commutative?  @x + y = y + x@
--
-- > > check $ isCommutative (+)
-- > +++ OK, passed 200 tests.
--
-- > > import Data.List
-- > > check $ isCommutative (union :: [Int]->[Int]->[Int])
-- > *** Failed! Falsifiable (after 4 tests):
-- > [] [0,0]
isCommutative :: Eq b => (a -> a -> b) -> a -> a -> Bool
isCommutative (?)  =  \x y -> x ? y == y ? x

{-# DEPRECATED commutative "Use isCommutative." #-}
commutative :: Eq b => (a -> a -> b) -> a -> a -> Bool
commutative  =  isCommutative

-- | Is a given operator associative?  @x + (y + z) = (x + y) + z@
--
-- > > check $ isAssociative (+)
-- > +++ OK, passed 200 tests.
--
-- > > check $ isAssociative (-)
-- > *** Failed! Falsifiable (after 2 tests):
-- > 0 0 1
isAssociative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
isAssociative (?)  =  \x y z -> x ? (y ? z) == (x ? y) ? z

{-# DEPRECATED associative "Use isAssociative." #-}
associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative  =  isAssociative

-- | Does the first operator, left-distributes over the second?
--
-- This is an alias to 'isLeftDistributiveOver'.
isDistributiveOver :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
isDistributiveOver  =  isLeftDistributiveOver

-- | Does the first operator, left-distributes over the second?
--   @x * (y + z) = (x * y) + (x * z)@
--
-- > > check $ (*) `isLeftDistributiveOver` (+)
-- > +++ OK, passed 200 tests.
--
-- > > check $ (+) `isLeftDistributiveOver` (*)
-- > *** Failed! Falsifiable (after 8 tests):
-- > 1 0 1
isLeftDistributiveOver :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
(?) `isLeftDistributiveOver` (#)  =  \x y z -> x ? (y # z) == (x ? y) # (x ? z)

-- | Does the first operator, right-distributes over the second?
--   @(y + z) * x = (y * x) + (z * x)@
--
-- > > check $ (*) `isRightDistributiveOver` (+)
-- > +++ OK, passed 200 tests.
--
-- > > check $ (+) `isRightDistributiveOver` (*)
-- > *** Failed! Falsifiable (after 8 tests):
-- > 1 0 1
isRightDistributiveOver :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
(?) `isRightDistributiveOver` (#)  =  \x y z -> (y # z) ? x == (y ? x) # (z ? x)

{-# DEPRECATED distributive "Use isDistributiveOver." #-}
distributive :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributive  =  isDistributiveOver

-- | Are two operators flipped versions of each other?
--
-- > holds n $ (<)  `symmetric2` (>)  -:> int
-- > holds n $ (<=) `symmetric2` (>=) -:> int
--
-- > fails n $ (<)  `symmetric2` (>=) -:> int
-- > fails n $ (<=) `symmetric2` (>)  -:> int
symmetric2 :: Eq c => (a -> b -> c) -> (b -> a -> c) -> a -> b -> Bool
symmetric2 (+-) (-+)  =  \x y -> x +- y == y -+ x
-- TODO: possible names: isFlipped, isSymmetricTo

-- | Is a given relation transitive?
--
-- A relation is transitive when
-- if a is related to b then b is related to c.
--
-- > > check $ isTransitive ((==) :: Int->Int->Bool)
-- > +++ OK, passed 200 tests.
--
-- > > check $ isTransitive ((/=) :: Int->Int->Bool)
-- > *** Failed! Falsifiable (after 3 tests):
-- > 0 1 0
isTransitive :: (a -> a -> Bool) -> a -> a -> a -> Bool
isTransitive (?)  =  \x y z -> x ? y && y ? z ==> x ? z

{-# DEPRECATED transitive "Use isTransitive." #-}
transitive :: (a -> a -> Bool) -> a -> a -> a -> Bool
transitive  =  isTransitive

-- | Is a given relation reflexive?
--
-- A relation is reflexive when
-- an element is always related to itself.
--
-- > > check $ isReflexive ((==) :: Int->Int->Bool)
-- > +++ OK, passed 200 tests.
--
-- > > check $ isReflexive ((/=) :: Int->Int->Bool)
-- > *** Failed! Falsifiable (after 1 tests):
-- > 0
isReflexive :: (a -> a -> Bool) -> a -> Bool
isReflexive (?)  =  \x -> x ? x

{-# DEPRECATED reflexive "Use isReflexive." #-}
reflexive :: (a -> a -> Bool) -> a -> Bool
reflexive  =  isReflexive

-- | Is a given relation irreflexive?
--
-- A given relation is irreflexive or anti-reflexive
-- when an element is _never_ related to itself.
--
-- This is different than "not reflexive".
--
-- > > check $ isIrreflexive ((==) :: Int->Int->Bool)
-- > *** Failed! Falsifiable (after 1 tests):
-- > 0
--
-- > > check $ isIrreflexive ((/=) :: Int->Int->Bool)
-- > +++ OK, passed 200 tests.
isIrreflexive :: (a -> a -> Bool) -> a -> Bool
isIrreflexive (?)  =  \x -> not $ x ? x

{-# DEPRECATED irreflexive "Use isIrreflexive." #-}
irreflexive :: (a -> a -> Bool) -> a -> Bool
irreflexive  =  isIrreflexive

-- | Is a given relation symmetric?
--
-- A relation is symmetric when
-- if a is related to b, then b is related to a.
--
-- > > check $ isSymmetric (&&)
-- > +++ OK, passed 4 tests (exhausted).
--
-- > > check $ isSymmetric (==>)
-- > *** Failed! Falsifiable (after 2 tests):
-- > False True
--
-- This is a type-restricted version of 'isCommutative'.
isSymmetric :: (a -> a -> Bool) -> a -> a -> Bool
isSymmetric  =  commutative

{-# DEPRECATED symmetric "Use isSymmetric." #-}
symmetric :: (a -> a -> Bool) -> a -> a -> Bool
symmetric  =  isSymmetric

-- | Is a given relation antisymmetric?
--
-- Not to be confused with not 'symmetric' or 'assymetric'.
--
-- > > check $ isAntisymmetric ((<=) :: Int->Int->Bool)
-- > +++ OK, passed 200 tests.
--
-- > > check $ isAntisymmetric ((/=) :: Int->Int->Bool)
-- > *** Failed! Falsifiable (after 2 tests):
-- > 0 1
isAntisymmetric :: Eq a => (a -> a -> Bool) -> a -> a -> Bool
isAntisymmetric (?)  =  \x y -> x ? y && y ? x ==> x == y

{-# DEPRECATED antisymmetric "Use isAntisymmetric." #-}
antisymmetric :: Eq a => (a -> a -> Bool) -> a -> a -> Bool
antisymmetric (?)  =  \x y -> x ? y && y ? x ==> x == y

-- | Is a given relation asymmetric?
--
-- Not to be confused with not 'symmetric' and 'antissymetric'.
--
-- > > check $ isAsymmetric ((<=) :: Int->Int->Bool)
-- > *** Failed! Falsifiable (after 1 tests):
-- > 0 0
--
-- > > check $ isAsymmetric ((<) :: Int->Int->Bool)
-- > +++ OK, passed 200 tests.
isAsymmetric :: (a -> a -> Bool) -> a -> a -> Bool
isAsymmetric (?)  =  \x y -> x ? y ==> not (y ? x)

{-# DEPRECATED asymmetric "Use isAsymmetric." #-}
asymmetric :: (a -> a -> Bool) -> a -> a -> Bool
asymmetric (?)  =  \x y -> x ? y ==> not (y ? x)

-- | Is the given binary relation an equivalence?
--
-- In other words,
-- is the given relation reflexive, symmetric and transitive?
--
-- > > check (isEquivalence (==) :: Int -> Int -> Int -> Bool)
-- > +++ OK, passed 200 tests.
--
-- > > check (isEquivalence (<=) :: Int -> Int -> Int -> Bool)
-- > *** Failed! Falsifiable (after 3 tests):
-- > 0 1 0
--
-- Or, using "Test.LeanCheck.Utils.TypeBinding":
--
-- > > check $ isEquivalence (<=) -:> int
-- > *** Failed! Falsifiable (after 3 tests):
-- > 0 1 0
isEquivalence :: (a -> a -> Bool) -> a -> a -> a -> Bool
isEquivalence (==)  =  \x y z -> reflexive  (==) x
                              && symmetric  (==) x y
                              && transitive (==) x y z

{-# DEPRECATED equivalence "Use isEquivalence." #-}
equivalence :: (a -> a -> Bool) -> a -> a -> a -> Bool
equivalence  =  isEquivalence

-- | Is the given binary relation a partial order?
--
-- In other words,
-- is the given relation reflexive, antisymmetric and transitive?
--
-- > > check $ isPartialOrder ((<) :: Int->Int->Bool)
-- > *** Failed! Falsifiable (after 1 tests):
-- > 0 0 0
--
-- > > check $ isPartialOrder ((<=) :: Int->Int->Bool)
-- > +++ OK, passed 200 tests.
--
-- > > check $ isPartialOrder isSubsetOf
-- > +++ OK, passed 200 tests.
isPartialOrder :: Eq a => (a -> a -> Bool) -> a -> a -> a -> Bool
isPartialOrder (<=)  =  \x y z -> reflexive     (<=) x
                               && antisymmetric (<=) x y
                               && transitive    (<=) x y z

{-# DEPRECATED partialOrder "Use isPartialOrder." #-}
partialOrder :: Eq a => (a -> a -> Bool) -> a -> a -> a -> Bool
partialOrder  =  isPartialOrder

-- | Is the given binary relation a strict partial order?
--
-- In other words,
-- is the given relation irreflexive, asymmetric and transitive?
--
-- > > check $ isStrictPartialOrder ((<) :: Int->Int->Bool)
-- > +++ OK, passed 200 tests.
--
-- > > check $ isStrictPartialOrder ((<=) :: Int->Int->Bool)
-- > *** Failed! Falsifiable (after 1 tests):
-- > 0 0 0
isStrictPartialOrder :: (a -> a -> Bool) -> a -> a -> a -> Bool
isStrictPartialOrder (<)  =  \x y z -> irreflexive (<) x
                                    && asymmetric  (<) x y -- implied?
                                    && transitive  (<) x y z

{-# DEPRECATED strictPartialOrder "Use isStrictPartialOrder." #-}
strictPartialOrder :: (a -> a -> Bool) -> a -> a -> a -> Bool
strictPartialOrder  =  isStrictPartialOrder

-- | Is the given binary relation a total order?
--
-- > > check $ isTotalOrder ((<) :: Int->Int->Bool)
-- > *** Failed! Falsifiable (after 1 tests):
-- > 0 0 0
-- > > check $ isTotalOrder ((<=) :: Int->Int->Bool)
-- > +++ OK, passed 200 tests.
isTotalOrder :: Eq a => (a -> a -> Bool) -> a -> a -> a -> Bool
isTotalOrder (<=)  =  \x y z -> (x <= y || y <= x)
                             && antisymmetric (<=) x y
                             && transitive    (<=) x y z

{-# DEPRECATED totalOrder "Use isTotalOrder." #-}
totalOrder :: Eq a => (a -> a -> Bool) -> a -> a -> a -> Bool
totalOrder (<=)  =  \x y z -> (x <= y || y <= x)
                           && antisymmetric (<=) x y
                           && transitive    (<=) x y z

-- | Is the given binary relation a strict total order?
strictTotalOrder :: Eq a => (a -> a -> Bool) -> a -> a -> a -> Bool
strictTotalOrder (<)  =  \x y z -> (x /= y ==> x < y || y < x)
                                && irreflexive (<) x
                                && asymmetric  (<) x y -- implied?
                                && transitive  (<) x y z

comparison :: (a -> a -> Ordering) -> a -> a -> a -> Bool
comparison compare  =  \x y z -> isEquivalence (===) x y z
                              && irreflexive (<) x
                              && transitive  (<) x y z
                              && symmetric2  (<) (>) x y
  where
  x === y  =  x `compare` y == EQ
  x  <  y  =  x `compare` y == LT
  x  >  y  =  x `compare` y == GT


-- | Is the given function idempotent? @f (f x) == x@
--
-- > > check $ idempotent abs
-- > +++ OK, passed 200 tests.
--
-- > > check $ idempotent sort
-- > +++ OK, passed 200 tests.
--
-- > > check $ idempotent negate
-- > *** Failed! Falsifiable (after 2 tests):
-- > 1
idempotent :: Eq a => (a -> a) -> a -> Bool
idempotent f  =  f . f === f

-- | Is the given function an identity? @f x == x@
--
-- > holds n $ identity (+0)
-- > holds n $ identity (sort :: [()])
-- > holds n $ identity (not . not)
identity :: Eq a => (a -> a) -> a -> Bool
identity f  =  f === id

-- | Is the given function never an identity? @f x /= x@
--
-- > holds n $ neverIdentity not
--
-- > fails n $ neverIdentity negate   -- yes, fails: negate 0 == 0, hah!
--
-- Note: this is not the same as not being an identity.
neverIdentity :: Eq a => (a -> a) -> a -> Bool
neverIdentity  =  (not .) . identity

okEq :: Eq a => a -> a -> a -> Bool
okEq  =  equivalence (==)

okOrd :: Ord a => a -> a -> a -> Bool
okOrd x y z  =  totalOrder (<=) x y z
             && comparison compare x y z
             && (x <= y) == ((x `compare` y) `elem` [LT,EQ])

okEqOrd :: (Eq a, Ord a) => a -> a -> a -> Bool
okEqOrd x y z  =  okEq  x y z
               && okOrd x y z
               && (x == y) == (x `compare` y == EQ) -- consistent instances

okNumNonNegative :: (Eq a, Num a) => a -> a -> a -> Bool
okNumNonNegative x y z  =  commutative (+) x y
                        && commutative (*) x y
                        && associative (+) x y z
                        && associative (*) x y z
                        && distributive (*) (+) x y z
                        && idempotent (+0) x
                        && idempotent (*1) x
                        && idempotent abs x
                        && idempotent signum x
                        && abs x * signum x == x

okNum :: (Eq a, Num a) => a -> a -> a -> Bool
okNum x y z  =  okNumNonNegative x y z
             && negate (negate x) == x
             && x - x == 0

-- | Equal under, a ternary operator with the same fixity as '=='.
--
-- > x =$ f $= y  =  f x = f y
--
-- > [1,2,3,4,5] =$  take 2    $= [1,2,4,8,16] -- > True
-- > [1,2,3,4,5] =$  take 3    $= [1,2,4,8,16] -- > False
-- >     [1,2,3] =$    sort    $= [3,2,1]      -- > True
-- >          42 =$ (`mod` 10) $= 16842        -- > True
-- >          42 =$ (`mod`  9) $= 16842        -- > False
-- >         'a' =$  isLetter  $= 'b'          -- > True
-- >         'a' =$  isLetter  $= '1'          -- > False
(=$) :: Eq b => a -> (a -> b) -> a -> Bool
(x =$ f) y  =  f x == f y
infixl 4 =$

-- | See '=$'
($=) :: (a -> Bool) -> a -> Bool
($=)  =  ($)
infixl 4 $=

-- | Check if two lists are equal for @n@ values.
--   This operator has the same fixity of '=='.
--
-- > xs =| n |= ys  =  take n xs == take n ys
--
-- > [1,2,3,4,5] =| 2 |= [1,2,4,8,16] -- > True
-- > [1,2,3,4,5] =| 3 |= [1,2,4,8,16] -- > False
(=|) :: Eq a => [a] -> Int -> [a] -> Bool
xs =| n  =  xs =$ take n
infixl 4 =|

-- | See '=|'
(|=) :: (a -> Bool) -> a -> Bool
(|=)  =  ($)
infixl 4 |=
