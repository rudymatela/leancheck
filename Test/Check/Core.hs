-- | Simple property-based testing library based on
--   enumeration of values via lists of lists.
--
-- This is the core module of the library, with the most basic definitions.  If
-- you are looking just to use the library, import and see "Test.Check".
--
-- If you want to understand how the code works, this is the place to start.
--
--
-- Other important modules:
--
-- "Test.Check.Basic" re-exports (almost) everything from this module
--         along with constructors and instances for further arities.
--
-- "Test.Check.Utils" re-exports "Test.Check.Basic"
--         along with functions for advanced Listable instance definitions.
--
-- "Test.Check" re-exports "Test.Check.Utils"
--   along with a TH function to automatically derive Listable instances.
module Test.Check.Core
  (
  -- * Checking and testing
    holds
  , fails
  , exists
  , counterExample
  , counterExamples
  , witness
  , witnesses
  , Testable

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

  -- ** Combining lists of tiers
  , (\/), (\\//)
  , (><)
  , tProduct
  , tProductWith

  -- ** Manipulating lists of tiers
  , tmap
  , tfilter
  , tFilter
  , tConcat
  , tConcatMap
  , toTiers

  -- ** Boolean (property) operators
  , (==>)

  -- ** Misc utilities
  , (+|)
  , (>:<)
  , productWith
  , listIntegral
  , tFractional
  )
where

import Data.Maybe (listToMaybe)


-- | A type is 'Listable' when there exists a function that
--   is able to list (ideally all of) its values.
--
-- Ideally, this type should be defined by a 'tiers' function that
-- returns a (possibly infinite) list of finite sub-lists (tiers):
--   the first sub-list contains elements of size 0,
--   the second sub-list contains elements of size 1
--   and so on.
-- Size here is defined by the implementor of the type-class instance.
--
-- For algebraic data types, the general form for 'tiers' is:
--
-- > tiers = consN ConstructorA
-- >      \/ consN ConstructorB
-- >      \/ consN ConstructorC
-- >      \/ ...
--
-- When defined by 'list', each sub-list in 'tiers' is a singleton list
-- (each element of 'list' has +1 size).
--
-- The function 'Test.Check.Derive.deriveListable' from "Test.Check.Derive"
-- can automatically derive instances of this typeclass.
--
-- A 'Listable' instance for functions is also available but is not exported by
-- default.  Import "Test.Check.Function" for that.
-- ("Test.Check.Function.Show" for a Show instance for functions)
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

instance (Listable a, Listable b) => Listable (a,b) where
  tiers = tProduct tiers tiers

instance (Listable a, Listable b, Listable c) => Listable (a,b,c) where
  tiers = tProductWith (\x (y,z) -> (x,y,z)) tiers tiers

instance (Listable a, Listable b, Listable c, Listable d) =>
         Listable (a,b,c,d) where
  tiers = tProductWith (\x (y,z,w) -> (x,y,z,w)) tiers tiers

instance (Listable a, Listable b, Listable c, Listable d, Listable e) =>
         Listable (a,b,c,d,e) where
  tiers = tProductWith (\x (y,z,w,v) -> (x,y,z,w,v)) tiers tiers

instance (Listable a) => Listable [a] where
  tiers = cons0 []
       \/ cons2 (:)

-- The position of Infinity in the enumeration is arbitrary.
tFractional :: Fractional a => [[a]]
tFractional = tProductWith (+) tFractionalParts
                               (tmap fromIntegral (tiers::[[Integer]]))
            \/ [ [], [], [1/0], [-1/0] {- , [-0], [0/0] -} ]
  where tFractionalParts :: Fractional a => [[a]]
        tFractionalParts = [0]
                         : [ [fromIntegral a / fromIntegral b]
                           | b <- iterate (*2) 2, a <- [1::Integer,3..b] ]

-- Note that this instance ignores NaN's.
instance Listable Float where
  tiers = tFractional

instance Listable Double where
  tiers = tFractional


-- | 'map' over tiers
tmap :: (a -> b) -> [[a]] -> [[b]]
tmap = map . map

-- | 'filter' tiers
tfilter :: (a -> Bool) -> [[a]] -> [[a]]
tfilter f = map (filter f)

-- | 'filter' tiers
tFilter :: (a -> Bool) -> [[a]] -> [[a]]
tFilter = tfilter

-- | 'concat' tiers of tiers
tConcat :: [[ [[a]] ]] -> [[a]]
tConcat = foldr (\+:/) [] . map (foldr (\/) [])
  where xss \+:/ yss = xss \/ ([]:yss)

-- | 'concatMap' over tiers
tConcatMap :: (a -> [[b]]) -> [[a]] -> [[b]]
tConcatMap f = tConcat . tmap f


-- | Takes a constructor with no arguments and return tiers (with a single value).
--   This value, by default, has size/weight 0.
cons0 :: a -> [[a]]
cons0 x = [[x]]

-- | Takes a constructor with one argument and return tiers of that value.
--   This value, by default, has size/weight 1.
cons1 :: Listable a => (a -> b) -> [[b]]
cons1 f = tmap f tiers `addWeight` 1

-- | Takes a constructor with two arguments and return tiers of that value.
--   This value, by default, has size/weight 1.
cons2 :: (Listable a, Listable b) => (a -> b -> c) -> [[c]]
cons2 f = tmap (uncurry f) tiers `addWeight` 1

cons3 :: (Listable a, Listable b, Listable c) => (a -> b -> c -> d) -> [[d]]
cons3 f = tmap (uncurry3 f) tiers `addWeight` 1

cons4 :: (Listable a, Listable b, Listable c, Listable d)
      => (a -> b -> c -> d -> e) -> [[e]]
cons4 f = tmap (uncurry4 f) tiers `addWeight` 1

cons5 :: (Listable a, Listable b, Listable c, Listable d, Listable e)
      => (a -> b -> c -> d -> e -> f) -> [[f]]
cons5 f = tmap (uncurry5 f) tiers `addWeight` 1

-- | Resets the weight of a constructor (or tiers)
-- Typically used as an infix constructor when defining Listable instances:
--
-- > cons<N> `ofWeight` W
--
-- Be careful: do not apply @`ofWeight` 0@ to recursive data structure
-- constructors.  In general this will make the list of size 0 infinite,
-- breaking the tier invariant (each tier must be finite).
ofWeight :: [[a]] -> Int -> [[a]]
ofWeight xss w = dropWhile null xss `addWeight` w

-- | Adds to the weight of tiers of a constructor
addWeight :: [[a]] -> Int -> [[a]]
addWeight xss w = replicate w [] ++ xss

-- | Lazily interleaves two lists, switching between elements of the two.
--   Union/sum of the elements in the lists.
--
-- > [x,y,z] +| [a,b,c] == [x,a,y,b,z,c]
(+|) :: [a] -> [a] -> [a]
[]     +| ys = ys
(x:xs) +| ys = x:(ys +| xs)
infixr 5 +|

-- | Product of two lists.
--
-- > [x,y] >:< [a,b] = [(x,a),(x,b),(y,a),(y,b)]
(>:<) :: [a] -> [b] -> [(a,b)]
xs >:< ys = [(x,y) | x <- xs, y <- ys]

-- | Product of two lists by a given function.
productWith :: (a->b->c) -> [a] -> [b] -> [c]
productWith f xs ys = [x `f` y | x <- xs, y <- ys]

-- | Append tiers.
--
-- > [xs,ys,zs,...] \/ [as,bs,cs,...] = [xs++as,ys++bs,zs++cs,...]
(\/) :: [[a]] -> [[a]] -> [[a]]
xss \/ []  = xss
[]  \/ yss = yss
(xs:xss) \/ (ys:yss) = (xs ++ ys) : xss \/ yss
infixr 7 \/

-- | Interleave tiers.  When in doubt, use @\/@ instead.
--
-- > [xs,ys,zs,...] \/ [as,bs,cs,...] = [xs+|as,ys+|bs,zs+|cs,...]
(\\//) :: [[a]] -> [[a]] -> [[a]]
xss \\// []  = xss
[]  \\// yss = yss
(xs:xss) \\// (ys:yss) = (xs +| ys) : xss \\// yss
infixr 7 \\//

-- | Take a tiered product of lists of tiers.
--
-- > tProduct [[0]..] [[0]..]
-- > == [  [(0,0)]
-- >    ,  [(1,0),(0,1)]
-- >    ,  [(2,0),(1,1),(0,2)]
-- >    ,  [(3,0),(2,1),(1,2),(0,3)]
-- >    ...
-- >    ]
--
-- In terms of '(>:<)', 'tsProduct' is:
--
-- > tProduct [xs] [ys] = [xs>:<ys]
-- > tProduct [xs0,xs1] [ys0] = [xs0>:<ys0, xs1>:<ys0]
-- > tProduct [xs0,xs1] [ys0,ys1] = [xs0>:<ys0, xs1>:<ys0++xs0>:<ys1, xs1>:<ys1]
-- > tProduct [xs0,xs1,xs2] [ys0,ys1] =
-- >   [ xs0 >:< ys0
-- >   , xs1 >:< ys0 ++ xs0 >:< ys1
-- >   , xs2 >:< ys0 ++ xs1 >:< ys1
-- >   ]
-- > tProduct [xs0,xs1,xs2] [ys0,ys1,ys2] =
-- >   [ xs0 >:< ys0
-- >   , xs1 >:< ys0 ++ xs0 >:< ys1
-- >   , xs2 >:< ys0 ++ xs1 >:< ys1 ++ xs0 >:< ys2
-- >   , xs2 >:< ys1 ++ xs1 >:< ys2
-- >   , xs2 >:< ys2
-- >   ]
-- > tProduct ...
tProduct :: [[a]] -> [[b]] -> [[(a,b)]]
tProduct = tProductWith (,)

-- | Infix shorthand for 'tsProduct'
(><) :: [[a]] -> [[b]] -> [[(a,b)]]
(><) = tProduct
infixr 8 ><

tProductWith :: (a->b->c) -> [[a]] -> [[b]] -> [[c]]
tProductWith _ _ [] = []
tProductWith _ [] _ = []
tProductWith f (xs:xss) yss = map (productWith f xs) yss
                           \/ tProductWith f xss yss `addWeight` 1


-- | 'Testable' values are functions
--   of 'Listable' arguments that return boolean values,
--   e.g.:
--
-- * @ Bool @
-- * @ Int -> Bool @
-- * @ Listable a => a -> a -> Bool @
class Testable a where
  tResults   :: a -> [[([String],Bool)]]

instance Testable Bool where
  tResults p = [[([],p)]]

instance (Testable b, Show a, Listable a) => Testable (a->b) where
  tResults p = tConcatMap tResultsFor tiers
    where tResultsFor x = mapFst (showsPrec 11 x "":) `tmap` tResults (p x)
          mapFst f (x,y) = (f x, y)

-- | List all results of a 'Testable' property.
-- Each results is composed by a list of strings and a boolean.
-- The list of strings represents the arguments applied to the function.
-- The boolean tells whether the property holds for that selection of argument.
-- This list is usually infinite.
results :: Testable a => a -> [([String],Bool)]
results = concat . tResults

-- | Lists all counter-examples for a number of tests to a property,
counterExamples :: Testable a => Int -> a -> [[String]]
counterExamples n = map fst . filter (not . snd) . take n . results

-- | For a number of tests to a property,
--   returns Just the first counter-example or Nothing.
counterExample :: Testable a => Int -> a -> Maybe [String]
counterExample n = listToMaybe . counterExamples n

-- | Lists all witnesses for a number of tests to a property,
witnesses :: Testable a => Int -> a -> [[String]]
witnesses n = map fst . filter snd . take n . results

-- | For a number of tests to a property,
--   returns Just the first witness or Nothing.
witness :: Testable a => Int -> a -> Maybe [String]
witness n = listToMaybe . witnesses n

-- | Does a property __hold__ for a number of test values?
--
-- > holds 1000 $ \xs -> length (sort xs) == length xs
holds :: Testable a => Int -> a -> Bool
holds n = and . take n . map snd . results

-- | Does a property __fail__ for a number of test values?
--
-- > fails 1000 $ \xs -> xs ++ ys == ys ++ xs
fails :: Testable a => Int -> a -> Bool
fails n = not . holds n

-- | There __exists__ and assignment of values that satisfy a property?
exists :: Testable a => Int -> a -> Bool
exists n = or . take n . map snd . results

uncurry3 :: (a->b->c->d) -> (a,b,c) -> d
uncurry3 f (x,y,z) = f x y z

uncurry4 :: (a->b->c->d->e) -> (a,b,c,d) -> e
uncurry4 f (x,y,z,w) = f x y z w

uncurry5 :: (a->b->c->d->e->f) -> (a,b,c,d,e) -> f
uncurry5 f (x,y,z,w,v) = f x y z w v

-- | Boolean implication.  Use this for defining conditional properties:
--
-- > prop_something x y = condition x y ==> something x y
(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p
infixr 0 ==>
