module Test.LeanCheck.Utils.Operators
  (
--  (==>) -- already provided by Test.LeanCheck

  -- * Combining properties
    (===), (====)
  , (&&&), (&&&&)
  , (|||), (||||)

  -- * Properties of unary functions
  , idempotent
  , identity
  , neverIdentity

  -- * Properties of operators (binary functions)
  , commutative
  , associative
  , distributive
  , symmetric2

  -- * Properties of relations (binary functions returning truth values)
  , transitive
  , reflexive
  , irreflexive
  , symmetric
  , asymmetric
  , antisymmetric

  -- ** Order relations
  , equivalence
  , partialOrder
  , strictPartialOrder
  , totalOrder
  , strictTotalOrder

  -- * Ternary comparison operators
  , (=$), ($=)
  , (=|), (|=)

  -- * Properties for typeclass instances
  , okEq
  , okOrd
  , okEqOrd
  )
where

-- TODO: review terminology in this module.  Some names aren't quite right!

import Test.LeanCheck ((==>))

combine :: (b -> c -> d) -> (a -> b) -> (a -> c) -> (a -> d)
combine op f g = \x -> f x `op` g x

-- Uneeded, just food for thought:
-- > combine2 :: (c -> d -> e) -> (a -> b -> c) -> (a -> b -> d) -> (a -> b -> e)
-- Two possible implementations:
-- > combine2 op f g = \x y -> f x y `op` g x y
-- > combine2 = combine . combine

(===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(===) = combine (==)
infix 4 ===

(====) :: Eq c => (a -> b -> c) -> (a -> b -> c) -> a -> b -> Bool
(====) = combine (===)
infix 4 ====

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = combine (&&)
infix 3 &&&

(&&&&) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
(&&&&) = combine (&&&)
infix 3 &&&&

(&&&&&) :: (a -> b -> c -> Bool) -> (a -> b -> c -> Bool) -> a -> b -> c -> Bool
(&&&&&) = combine (&&&&)
infix 2 &&&&&

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = combine (||)
infix 2 |||

(||||) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
(||||) = combine (|||)
infix 2 ||||

-- | Is a given operator commutative?  @x + y = y + x@
--
-- > holds n $ commutative (+)
--
-- > fails n $ commutative union  -- union [] [0,0] = [0]
commutative :: Eq b => (a -> a -> b) -> a -> a -> Bool
commutative o = \x y -> x `o` y == y `o` x

-- | Is a given operator associative?  @x + (y + z) = (x + y) + z@
associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative o = \x y z -> x `o` (y `o` z) == (x `o` y) `o` z

-- | Does the first operator, distributes over the second?
distributive :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributive o o' = \x y z -> x `o` (y `o'` z) == (x `o` y) `o'` (x `o` z)

-- | Are two operators flipped versions of each other?
--
-- > holds n $ (<)  `symmetric2` (>)  -:> int
-- > holds n $ (<=) `symmetric2` (>=) -:> int
--
-- > fails n $ (<)  `symmetric2` (>=) -:> int
-- > fails n $ (<=) `symmetric2` (>)  -:> int
symmetric2 :: Eq b => (a -> a -> b) -> (a -> a -> b) -> a -> a -> Bool
symmetric2 (+-) (-+) = \x y -> x +- y == y -+ x
-- TODO: generalize type of symmetric2!  a -> b -> c, b -> a -> c

-- | Is a given relation transitive?
transitive :: (a -> a -> Bool) -> a -> a -> a -> Bool
transitive o = \x y z -> x `o` y && y `o` z ==> x `o` z

-- | An element is always related to itself.
reflexive :: (a -> a -> Bool) -> a -> Bool
reflexive o = \x -> x `o` x

-- | An element is __never__ related to itself.
irreflexive :: (a -> a -> Bool) -> a -> Bool
irreflexive o = \x -> not $ x `o` x

-- | Is a given relation symmetric?
-- This is a type-restricted version of 'commutative'.
symmetric :: (a -> a -> Bool) -> a -> a -> Bool
symmetric = commutative

-- | Is a given relation antisymmetric?
-- Not to be confused with "not symmetric" and "assymetric".
antisymmetric :: Eq a => (a -> a -> Bool) -> a -> a -> Bool
antisymmetric r = \x y -> x `r` y && y `r` x ==> x == y

-- | Is a given relation asymmetric?
-- Not to be confused with "not symmetric" and "antissymetric".
asymmetric :: (a -> a -> Bool) -> a -> a -> Bool
asymmetric r = \x y -> x `r` y ==> not (y `r` x)

equivalence :: (a -> a -> Bool) -> a -> a -> a -> Bool
equivalence (==) = \x y z -> reflexive  (==) x
                          && symmetric  (==) x y
                          && transitive (==) x y z

partialOrder :: Eq a => (a -> a -> Bool) -> a -> a -> a -> Bool
partialOrder (<=) = \x y z -> reflexive     (<=) x
                           && antisymmetric (<=) x y
                           && transitive    (<=) x y z

strictPartialOrder :: Eq a => (a -> a -> Bool) -> a -> a -> a -> Bool
strictPartialOrder (<) = \x y z -> irreflexive   (<) x
                                && antisymmetric (<) x y
                                && transitive    (<) x y z

totalOrder :: Eq a => (a -> a -> Bool) -> a -> a -> a -> Bool
totalOrder (<=) = \x y z -> (x <= y || y <= x)
                         && antisymmetric (<=) x y
                         && transitive    (<=) x y z

strictTotalOrder :: Eq a => (a -> a -> Bool) -> a -> a -> a -> Bool
strictTotalOrder (<) = \x y z -> (x /= y ==> x < y || y < x)
                              && irreflexive   (<) x
                              && antisymmetric (<) x y
                              && transitive    (<) x y z


-- | Is the given function idempotent? @f (f x) == x@
--
-- > holds n $ idempotent abs
-- > holds n $ idempotent sort
--
-- > fails n $ idempotent negate
idempotent :: Eq a => (a -> a) -> a -> Bool
idempotent f = f . f === f

-- | Is the given function an identity? @f x == x@
--
-- > holds n $ identity (+0)
-- > holds n $ identity (sort :: [()])
-- > holds n $ identity (not . not)
identity :: Eq a => (a -> a) -> a -> Bool
identity f = f === id

-- | Is the given function never an identity? @f x /= x@
--
-- > holds n $ neverIdentity not
--
-- > fails n $ neverIdentity negate   -- yes, fails: negate 0 == 0, hah!
--
-- Note: this is not the same as not being an identity.
neverIdentity :: Eq a => (a -> a) -> a -> Bool
neverIdentity = (not .) . identity

okEq :: Eq a => a -> a -> a -> Bool
okEq = equivalence (==)

okOrd :: Ord a => a -> a -> a -> Bool
okOrd = totalOrder (<=)
  &&&&& \x y z -> True -- TODO: comparison compare

okEqOrd :: (Eq a, Ord a) => a -> a -> a -> Bool
okEqOrd = okEq
    &&&&& okOrd

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
(x =$ f) y = f x == f y
infixl 4 =$

-- | See '=$'
($=) :: (a -> Bool) -> a -> Bool
($=) = ($)
infixl 4 $=

-- | Check if two lists are equal for @n@ values.
--   This operator has the same fixity of '=='.
--
-- > xs =| n |= ys  =  take n xs == take n ys
--
-- > [1,2,3,4,5] =| 2 |= [1,2,4,8,16] -- > True
-- > [1,2,3,4,5] =| 3 |= [1,2,4,8,16] -- > False
(=|) :: Eq a => [a] -> Int -> [a] -> Bool
xs =| n = xs =$ take n
infixl 4 =|

-- | See '=|'
(|=) :: (a -> Bool) -> a -> Bool
(|=) = ($)
infixl 4 |=
