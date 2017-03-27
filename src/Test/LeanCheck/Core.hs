-- |
-- Module      : Test.LeanCheck.Core
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- LeanCheck is a simple enumerative property-based testing library.
--
-- This is the core module of the library, with the most basic definitions.  If
-- you are looking just to use the library, import and see "Test.LeanCheck".
--
-- If you want to understand how the code works, this is the place to start
-- reading.
--
--
-- Other important modules:
--
-- * "Test.LeanCheck.Basic" exports:
--     "Test.LeanCheck.Core",
--     additional 'tiers' constructors
--       ('Test.LeanCheck.Basic.cons6' ...
--        'Test.LeanCheck.Basic.cons12') and
--     'Listable' tuple instances.
--
-- * "Test.LeanCheck.Tiers" exports:
--     functions for advanced Listable definitions.
--
-- * "Test.LeanCheck" exports:
--      "Test.LeanCheck.Basic",
--      most of "Test.LeanCheck.Tiers" and
--      'Test.LeanCheck.Derive.deriveListable'.
module Test.LeanCheck.Core
  (
  -- * Checking and testing
    holds
  , fails
  , exists
  , counterExample
  , counterExamples
  , witness
  , witnesses
  , Testable(..)

  , results

  -- * Listing test values
  , Listable(..)

  -- ** Constructing lists of tiers
  , cons0
  , cons1
  , cons2
  , cons3
  , cons4
  , cons5

  , ofWeight
  , addWeight
  , suchThat

  -- ** Combining lists of tiers
  , (\/), (\\//)
  , (><)
  , productWith

  -- ** Manipulating lists of tiers
  , mapT
  , filterT
  , concatT
  , concatMapT
  , toTiers

  -- ** Boolean (property) operators
  , (==>)

  -- ** Misc utilities
  , (+|)
  , listIntegral
  , tiersFractional
  )
where

import Data.Maybe (listToMaybe)


-- | A type is 'Listable' when there exists a function that
--   is able to list (ideally all of) its values.
--
-- Ideally, instances should be defined by a 'tiers' function that
-- returns a (potentially infinite) list of finite sub-lists (tiers):
--   the first sub-list contains elements of size 0,
--   the second sub-list contains elements of size 1
--   and so on.
-- Size here is defined by the implementor of the type-class instance.
--
-- For algebraic data types, the general form for 'tiers' is
--
-- > tiers = cons<N> ConstructorA
-- >      \/ cons<N> ConstructorB
-- >      \/ ...
-- >      \/ cons<N> ConstructorZ
--
-- where @N@ is the number of arguments of each constructor @A...Z@.
--
-- Instances can be alternatively defined by 'list'.
-- In this case, each sub-list in 'tiers' is a singleton list
-- (each succeeding element of 'list' has +1 size).
--
-- The function 'Test.LeanCheck.Derive.deriveListable' from "Test.LeanCheck.Derive"
-- can automatically derive instances of this typeclass.
--
-- A 'Listable' instance for functions is also available but is not exported by
-- default.  Import "Test.LeanCheck.Function" if you need to test higher-order
-- properties.
class Listable a where
  tiers :: [[a]]
  list :: [a]
  tiers = toTiers list
  list = concat tiers
  {-# MINIMAL list | tiers #-}

-- | Takes a list of values @xs@ and transform it into tiers on which each
--   tier is occupied by a single element from @xs@.
--
-- To convert back to a list, just 'concat'.
toTiers :: [a] -> [[a]]
toTiers = map (:[])

instance Listable () where
  list = [()]

-- | Tiers of 'Integral' values.
--   Can be used as a default implementation of 'list' for 'Integral' types.
listIntegral :: (Enum a, Num a) => [a]
listIntegral = [0,-1..] +| [1..]

instance Listable Int where
  list = listIntegral

instance Listable Integer where
  list = listIntegral

instance Listable Char where
  list = ['a'..'z']
      +| [' ','\n']
      +| ['A'..'Z']
      +| ['0'..'9']
      +| ['!'..'/']
      +| ['\t']
      +| [':'..'@']
      +| ['['..'`']
      +| ['{'..'~']

instance Listable Bool where
  tiers = cons0 False \/ cons0 True

instance Listable a => Listable (Maybe a) where
  tiers = cons0 Nothing \/ cons1 Just

instance (Listable a, Listable b) => Listable (Either a b) where
  tiers = cons1 Left  `ofWeight` 0
     \\// cons1 Right `ofWeight` 0

-- | > list :: [(Int,Int)] = [(0,0), (0,1), (1,0), (0,-1), (1,1), ...]
instance (Listable a, Listable b) => Listable (a,b) where
  tiers = tiers >< tiers

instance (Listable a, Listable b, Listable c) => Listable (a,b,c) where
  tiers = productWith (\x (y,z) -> (x,y,z)) tiers tiers

instance (Listable a, Listable b, Listable c, Listable d) =>
         Listable (a,b,c,d) where
  tiers = productWith (\x (y,z,w) -> (x,y,z,w)) tiers tiers

-- | Instances for 'Listable' sixtuples up to 12-tuples are exported by default
--   form "Test.LeanCheck" but are hidden from Haddock documentation.  These
--   instances are defined in "Test.LeanCheck.Basic".
instance (Listable a, Listable b, Listable c, Listable d, Listable e) =>
         Listable (a,b,c,d,e) where
  tiers = productWith (\x (y,z,w,v) -> (x,y,z,w,v)) tiers tiers

instance (Listable a) => Listable [a] where
  tiers = cons0 []
       \/ cons2 (:)

-- | Tiers of 'Fractional' values.
--   This can be used as the implementation of 'tiers' for 'Fractional' types.
tiersFractional :: Fractional a => [[a]]
tiersFractional = productWith (+) tiersFractionalParts
                                  (mapT fromIntegral (tiers::[[Integer]]))
               \/ [ [], [], [1/0], [-1/0] {- , [-0], [0/0] -} ]
  where tiersFractionalParts :: Fractional a => [[a]]
        tiersFractionalParts = [0]
                             : [ [fromIntegral a / fromIntegral b]
                               | b <- iterate (*2) 2, a <- [1::Integer,3..b] ]
-- The position of Infinity in the above enumeration is arbitrary.

-- Note that this instance ignores NaN's.
instance Listable Float where
  tiers = tiersFractional

instance Listable Double where
  tiers = tiersFractional

instance Listable Ordering where
  tiers = cons0 LT
       \/ cons0 EQ
       \/ cons0 GT

-- | 'map' over tiers
mapT :: (a -> b) -> [[a]] -> [[b]]
mapT = map . map

-- | 'filter' tiers
filterT :: (a -> Bool) -> [[a]] -> [[a]]
filterT f = map (filter f)

-- | 'concat' tiers of tiers
concatT :: [[ [[a]] ]] -> [[a]]
concatT = foldr (\+:/) [] . map (foldr (\/) [])
  where xss \+:/ yss = xss \/ ([]:yss)

-- | 'concatMap' over tiers
concatMapT :: (a -> [[b]]) -> [[a]] -> [[b]]
concatMapT f = concatT . mapT f


-- | Given a constructor with no arguments,
--   returns 'tiers' of all possible applications of this constructor.
--   Since in this case there is only one possible application (to no
--   arguments), only a single value, of size/weight 0, will be present in the
--   resulting list of tiers.
cons0 :: a -> [[a]]
cons0 x = [[x]]

-- | Given a constructor with one 'Listable' argument,
--   return 'tiers' of applications of this constructor.
--   By default, returned values will have size/weight of 1.
cons1 :: Listable a => (a -> b) -> [[b]]
cons1 f = mapT f tiers `addWeight` 1

-- | Given a constructor with two 'Listable' arguments,
--   return 'tiers' of applications of this constructor.
--   By default, returned values will have size/weight of 1.
cons2 :: (Listable a, Listable b) => (a -> b -> c) -> [[c]]
cons2 f = mapT (uncurry f) tiers `addWeight` 1

-- | Returns tiers of applications of a 3-argument constructor.
cons3 :: (Listable a, Listable b, Listable c) => (a -> b -> c -> d) -> [[d]]
cons3 f = mapT (uncurry3 f) tiers `addWeight` 1

-- | Returns tiers of applications of a 4-argument constructor.
cons4 :: (Listable a, Listable b, Listable c, Listable d)
      => (a -> b -> c -> d -> e) -> [[e]]
cons4 f = mapT (uncurry4 f) tiers `addWeight` 1

-- | Returns tiers of applications of a 5-argument constructor.
--
-- "Test.LeanCheck.Basic" defines
-- 'Test.LeanCheck.Basic.cons6' up to 'Test.LeanCheck.Basic.cons12'.
-- Those are exported by default from "Test.LeanCheck",
-- but are hidden from the Haddock documentation.
cons5 :: (Listable a, Listable b, Listable c, Listable d, Listable e)
      => (a -> b -> c -> d -> e -> f) -> [[f]]
cons5 f = mapT (uncurry5 f) tiers `addWeight` 1

-- | Resets the weight of a constructor (or tiers)
-- Typically used as an infix constructor when defining Listable instances:
--
-- > cons<N> `ofWeight` <W>
--
-- Be careful: do not apply @`ofWeight` 0@ to recursive data structure
-- constructors.  In general this will make the list of size 0 infinite,
-- breaking the tier invariant (each tier must be finite).
ofWeight :: [[a]] -> Int -> [[a]]
ofWeight xss w = dropWhile null xss `addWeight` w

-- | Adds to the weight of tiers of a constructor
addWeight :: [[a]] -> Int -> [[a]]
addWeight xss w = replicate w [] ++ xss

-- | Tiers of values that follow a property
--
-- > cons<N> `suchThat` condition
suchThat :: [[a]] -> (a->Bool) -> [[a]]
suchThat = flip filterT

-- | Lazily interleaves two lists, switching between elements of the two.
--   Union/sum of the elements in the lists.
--
-- > [x,y,z] +| [a,b,c] == [x,a,y,b,z,c]
(+|) :: [a] -> [a] -> [a]
[]     +| ys = ys
(x:xs) +| ys = x:(ys +| xs)
infixr 5 +|

-- | Append tiers --- sum of two tiers enumerations.
--
-- > [xs,ys,zs,...] \/ [as,bs,cs,...] = [xs++as,ys++bs,zs++cs,...]
(\/) :: [[a]] -> [[a]] -> [[a]]
xss \/ []  = xss
[]  \/ yss = yss
(xs:xss) \/ (ys:yss) = (xs ++ ys) : xss \/ yss
infixr 7 \/

-- | Interleave tiers --- sum of two tiers enumerations.
--   When in doubt, use '\/' instead.
--
-- > [xs,ys,zs,...] \/ [as,bs,cs,...] = [xs+|as,ys+|bs,zs+|cs,...]
(\\//) :: [[a]] -> [[a]] -> [[a]]
xss \\// []  = xss
[]  \\// yss = yss
(xs:xss) \\// (ys:yss) = (xs +| ys) : xss \\// yss
infixr 7 \\//

-- | Take a tiered product of lists of tiers.
--
-- > [t0,t1,t2,...] >< [u0,u1,u2,...] =
-- > [ t0**u0
-- > , t0**u1 ++ t1**u0
-- > , t0**u2 ++ t1**u1 ++ t2**u0
-- > , ...       ...       ...       ...
-- > ]
-- > where xs ** ys = [(x,y) | x <- xs, y <- ys]
--
-- Example:
--
-- > [[0],[1],[2],...] >< [[0],[1],[2],...]
-- > == [  [(0,0)]
-- >    ,  [(1,0),(0,1)]
-- >    ,  [(2,0),(1,1),(0,2)]
-- >    ,  [(3,0),(2,1),(1,2),(0,3)]
-- >    ...
-- >    ]
(><) :: [[a]] -> [[b]] -> [[(a,b)]]
(><) = productWith (,)
infixr 8 ><

-- | Take a tiered product of lists of tiers.
--   'productWith' can be defined by '><', as:
--
-- > productWith f xss yss = map (uncurry f) $ xss >< yss
productWith :: (a->b->c) -> [[a]] -> [[b]] -> [[c]]
productWith _ _ [] = []
productWith _ [] _ = []
productWith f (xs:xss) yss = map (xs **) yss
                          \/ productWith f xss yss `addWeight` 1
  where xs ** ys = [x `f` y | x <- xs, y <- ys]

-- | 'Testable' values are functions
--   of 'Listable' arguments that return boolean values,
--   e.g.:
--
-- * @ Bool @
-- * @ Listable a => a -> Bool @
-- * @ Listable a => a -> a -> Bool @
-- * @ Int -> Bool @
-- * @ String -> [Int] -> Bool @
class Testable a where
  resultiers :: a -> [[([String],Bool)]]

instance Testable Bool where
  resultiers p = [[([],p)]]

instance (Testable b, Show a, Listable a) => Testable (a->b) where
  resultiers p = concatMapT resultiersFor tiers
    where resultiersFor x = mapFst (showsPrec 11 x "":) `mapT` resultiers (p x)
          mapFst f (x,y) = (f x, y)

-- | List all results of a 'Testable' property.
-- Each result is a pair of a list of strings and a boolean.
-- The list of strings is a printable representation of one possible choice of
-- argument values for the property.  Each boolean paired with such a list
-- indicates whether the property holds for this choice.  The outer list is
-- potentially infinite and lazily evaluated.
results :: Testable a => a -> [([String],Bool)]
results = concat . resultiers

-- | Lists all counter-examples for a number of tests to a property,
counterExamples :: Testable a => Int -> a -> [[String]]
counterExamples n = map fst . filter (not . snd) . take n . results

-- | Up to a number of tests to a property,
--   returns 'Just' the first counter-example
--   or 'Nothing' if there is none.
--
-- > counterExample 100 $ \xs -> [] `union` xs == (xs::[Int])
-- > -- > Just ["[0,0]"]
counterExample :: Testable a => Int -> a -> Maybe [String]
counterExample n = listToMaybe . counterExamples n

-- | Lists all witnesses up to a number of tests to a property,
witnesses :: Testable a => Int -> a -> [[String]]
witnesses n = map fst . filter snd . take n . results

-- | Up to a number of tests to a property,
--   returns 'Just' the first witness
--   or 'Nothing' if there is none.
witness :: Testable a => Int -> a -> Maybe [String]
witness n = listToMaybe . witnesses n

-- | Does a property __hold__ up to a number of test values?
--
-- > holds 1000 $ \xs -> length (sort xs) == length xs
holds :: Testable a => Int -> a -> Bool
holds n = and . take n . map snd . results

-- | Does a property __fail__ for a number of test values?
--
-- > fails 1000 $ \xs -> xs ++ ys == ys ++ xs
fails :: Testable a => Int -> a -> Bool
fails n = not . holds n

-- | There __exists__ an assignment of values that satisfies a property
--   up to a number of test values?
--
-- > exists 1000 $ \x -> x > 10
exists :: Testable a => Int -> a -> Bool
exists n = or . take n . map snd . results

uncurry3 :: (a->b->c->d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z

uncurry4 :: (a->b->c->d->e) -> (a,b,c,d) -> e
uncurry4 f (x,y,z,w) = f x y z w

uncurry5 :: (a->b->c->d->e->f) -> (a,b,c,d,e) -> f
uncurry5 f (x,y,z,w,v) = f x y z w v

-- | Boolean implication operator.  Useful for defining conditional properties:
--
-- > prop_something x y = condition x y ==> something x y
(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p
infixr 0 ==>
