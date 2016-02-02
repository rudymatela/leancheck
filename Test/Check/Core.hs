-- | Very simple property-based testing library based on enumeration of values
--   via lists of lists.
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
  , arguments
  , resultArguments

  -- * Listing test values
  , Listable(..)

  -- ** Default-weighed listing constructors
  , cons0
  , cons1
  , cons2
  , cons3
  , cons4
  , cons5

  -- ** Custom-weighed listing constructors
  , wcons0
  , wcons1
  , wcons2
  , wcons3
  , wcons4
  , wcons5

  -- ** Combining listings
  , (\\//), (\++/)
  , lsProduct
  , lsProductWith

  -- ** Manipulating listings
  , lsmap
  , lsfilter
  , lsConcat
  , lsConcatMap
  , toListing

  -- ** Boolean (property) operators
  , (==>)

  -- ** Misc utilities
  , (\/)
  , (><)
  , productWith
  )
where

import Data.Maybe (listToMaybe)

-- | Minimal complete definition: listing or list
class Listable a where
  listing :: [[a]]
  list :: [a]
  listing = toListing list
  list = concat listing

-- | Takes a list of values @xs@ and transform it into a 'Listing' on which each
--   size is occupied by a single element from @xs@. 
--
-- To convert back to a list, just 'concat'.
toListing :: [a] -> [[a]]
toListing xs = map (:[]) xs

instance Listable () where
  list = [()]

instance Listable Int where
  list = [0,-1..] \/ [1..]

instance Listable Integer where
  list = [0,-1..] \/ [1..]

instance Listable Char where
  list = ['a'..'z']
      \/ [' ','\n']
      \/ ['A'..'Z']
      \/ ['0'..'9']
      \/ ['!'..'/']
      \/ ['\t']
      \/ [':'..'@']
      \/ ['['..'`']
      \/ ['{'..'~']


-- The position of Infinity in the enumeration is arbitrary.
lsFractional :: Fractional a => [[a]]
lsFractional = lsProductWith (+) lsFractionalParts
                                 (lsmap (fromIntegral) (listing::[[Integer]]))
          \++/ [ [], [], [1/0], [-1/0] {- , [-0], [0/0] -} ]
  where lsFractionalParts :: Fractional a => [[a]]
        lsFractionalParts = [0]
                          : [ [fromIntegral a / fromIntegral b]
                            | b <- iterate (*2) 2, a <- [1::Integer,3..b] ]

-- Note that this instance ignores NaN's.
instance Listable Float where
  listing = lsFractional

instance Listable Double where
  listing = lsFractional

instance Listable Bool where
  listing = cons0 False \++/ cons0 True

instance Listable a => Listable (Maybe a) where
  listing = cons0 Nothing \++/ cons1 Just

instance (Listable a, Listable b) => Listable (Either a b) where
  listing = wcons1 0 Left \\// wcons1 0 Right

instance (Listable a, Listable b) => Listable (a,b) where
  listing = lsProduct listing listing

instance (Listable a, Listable b, Listable c) => Listable (a,b,c) where
  listing = lsProductWith (\x (y,z) -> (x,y,z)) listing listing

instance (Listable a, Listable b, Listable c, Listable d) =>
         Listable (a,b,c,d) where
  listing = lsProductWith (\x (y,z,w) -> (x,y,z,w)) listing listing

instance (Listable a, Listable b, Listable c, Listable d, Listable e) =>
         Listable (a,b,c,d,e) where
  listing = lsProductWith (\x (y,z,w,v) -> (x,y,z,w,v)) listing listing

instance (Listable a) => Listable [a] where
  listing = cons0 []
       \++/ cons2 (:)

lsmap :: (a -> b) -> [[a]] -> [[b]]
lsmap = map . map

lsfilter :: (a -> Bool) -> [[a]] -> [[a]]
lsfilter f = map (filter f)

-- TODO: Just thinking: maybe have rcons and cons, where r is for recursive.
-- Generally, when a constructor is not allowed to be applied recursively (like
-- Just), it can add "zero" to size.  E.g.: Just
--
-- When it is recursive, it has to add "one" to size.  E.g.: Peano, (:)
wcons0 :: Int -> a -> [[a]]
wcons0 w x = replicate w [] ++ [[x]]

wcons1 :: Listable a => Int -> (a -> b) -> [[b]]
wcons1 w f = replicate w [] ++ lsmap f listing

wcons2 :: (Listable a, Listable b) => Int -> (a -> b -> c) -> [[c]]
wcons2 w f = replicate w [] ++ lsmap (uncurry f) listing

wcons3 :: (Listable a, Listable b, Listable c)
       => Int -> (a -> b -> c -> d) -> [[d]]
wcons3 w f = replicate w [] ++ lsmap (uncurry3 f) listing

wcons4 :: (Listable a, Listable b, Listable c, Listable d)
       => Int -> (a -> b -> c -> d -> e) -> [[e]]
wcons4 w f = replicate w [] ++ lsmap (uncurry4 f) listing

wcons5 :: (Listable a, Listable b, Listable c, Listable d, Listable e)
       => Int -> (a -> b -> c -> d -> e -> f) -> [[f]]
wcons5 w f = replicate w [] ++ lsmap (uncurry5 f) listing

cons0 :: a -> [[a]]
cons0 = wcons0 0

cons1 :: Listable a => (a -> b) -> [[b]]
cons1 = wcons1 1

cons2 :: (Listable a, Listable b) => (a -> b -> c) -> [[c]]
cons2 = wcons2 1

cons3 :: (Listable a, Listable b, Listable c) => (a -> b -> c -> d) -> [[d]]
cons3 = wcons3 1

cons4 :: (Listable a, Listable b, Listable c, Listable d)
      => (a -> b -> c -> d -> e) -> [[e]]
cons4 = wcons4 1

cons5 :: (Listable a, Listable b, Listable c, Listable d, Listable e)
      => (a -> b -> c -> d -> e -> f) -> [[f]]
cons5 = wcons5 1


-- | Lazily interleaves two lists, switching between elements of the two.
--   Union/sum of the elements in the lists.
(\/) :: [a] -> [a] -> [a]
[]     \/ ys     = ys
(x:xs) \/ ys     = x:(ys \/ xs)
infixr 5 \/

-- | Interleave values of each increasing size
(\\//) :: [[a]] -> [[a]] -> [[a]]
(\\//) = zipWith' (\/) [] []

-- | Append values of each increasing size
(\++/) :: [[a]] -> [[a]] -> [[a]]
(\++/) = zipWith' (++) [] []

(><) :: [a] -> [b] -> [(a,b)]
[]     ><  _  = []
(x:xs) >< ys = map ((,) x) ys ++ xs >< ys

productWith :: (a->b->c) -> [a] -> [b] -> [c]
productWith _ []     _   =  []
productWith f (x:xs) ys  =  map (f x) ys ++ productWith f xs ys

-- | 'zipwith\'' works similarly to 'zipWith', but takes neutral elements to
--   operate, so you don't loose elements.
--
-- > zipWith' f z e [x,y] [a,b,c,d] == [f x a, f y b, f z c, f z d]
--
-- > zipWith' f z e [x,y,z] [a] == [f x a, f y e, f z e]
--
-- > zipWith' (+) 0 0 [1,2,3] [1,2,3,4,5,6] == [2,4,6,4,5,6]
zipWith' :: (a->b->c) -> a -> b  -> [a] -> [b] -> [c]
zipWith' _ _  _  []     [] = []
zipWith' f _  zy xs     [] = map (`f` zy) xs
zipWith' f zx _  []     ys = map (f zx) ys
zipWith' f zx zy (x:xs) (y:ys) = f x y : zipWith' f zx zy xs ys

-- | Sized product of two lists.
--
-- > lsProduct [[0]..] [[0]..]
-- > == [  [(0,0)]
-- >    ,  [(1,0),(0,1)]
-- >    ,  [(2,0),(1,1),(0,2)]
-- >    ,  [(3,0),(2,1),(1,2),(0,3)]
-- >    ...
-- >    ]
--
-- In terms of ><, 'lsProduct' is:
--
-- > lsProduct [xs] [ys] = [xs><ys]
-- > lsProduct [xs0,xs1] [ys0] = [xs0><ys0, xs1><ys0]
-- > lsProduct [xs0,xs1] [ys0,ys1] = [xs0><ys0, xs1><ys0++xs0><ys1, xs1><ys1]
-- > lsProduct [xs0,xs1,xs2] [ys0,ys1] =
-- >                           [ xs0 >< ys0
-- >                           , xs1 >< ys0 ++ xs0 >< ys1
-- >                           , xs2 >< ys0 ++ xs1 >< ys1
-- >                           ]
-- > lsProduct [xs0,xs1,xs2] [ys0,ys1,ys2] =
-- >                           [ xs0 >< ys0
-- >                           , xs1 >< ys0 ++ xs0 >< ys1
-- >                           , xs2 >< ys0 ++ xs1 >< ys1 ++ xs0 >< ys2
-- >                           , xs2 >< ys1 ++ xs1 >< ys2
-- >                           , xs2 >< ys2
-- >                           ]
lsProduct :: [[a]] -> [[b]] -> [[(a,b)]]
lsProduct = lsProductWith (,)

lsProductWith :: (a->b->c) -> [[a]] -> [[b]] -> [[c]]
lsProductWith _ _ [] = []
lsProductWith _ [] _ = []
lsProductWith f (xs:xss) yss = zs  :  zss \++/ lsProductWith f xss yss
  where (zs:zss) = map (productWith f xs) yss

lsConcat :: [[ [[a]] ]] -> [[a]]
lsConcat = foldr (\+:/) [] . map (foldr (\++/) [])
  where xss \+:/ yss = xss \++/ ([]:yss)

lsConcatMap :: (a -> [[b]]) -> [[a]] -> [[b]]
lsConcatMap f = lsConcat . lsmap f

class Testable a where
  lsResults   :: a -> [[Bool]]
  lsArguments :: a -> [[[String]]]

instance Testable Bool where
  lsResults   p = [[p]]
  lsArguments p = [[[]]]

instance (Testable b, Show a, Listable a) => Testable (a->b) where
  lsResults   p = lsConcatMap (lsResults . p) listing
  lsArguments p = lsConcatMap (\x -> (showsPrec 11 x "":) `lsmap` lsArguments (p x)) listing

results :: Testable a => a -> [Bool]
results = concat . lsResults

arguments :: Testable a => a -> [[String]]
arguments = concat . lsArguments

resultArguments :: Testable a => a -> [(Bool,[String])]
resultArguments p = zip (results p) (arguments p)


-- | Returns the list of all counterexamples for a given property.
counterExamples :: Testable a => Int -> a -> [[String]]
counterExamples n = map snd . filter (not . fst) . take n . resultArguments

-- | For a given property, returns 'Just' the string description of the first
--   counterexample or 'Nothing'.
counterExample :: Testable a => Int -> a -> Maybe [String]
counterExample n = listToMaybe . counterExamples n

-- | Returns the list of all witnesses for a given property.
witnesses :: Testable a => Int -> a -> [[String]]
witnesses n = map snd . filter (fst) . take n . resultArguments

-- | For a given property up to some values,
--   returns Just the first witness or Nothing.
witness :: Testable a => Int -> a -> Maybe [String]
witness n = listToMaybe . witnesses n

-- | Check if a property holds up to some values.
holds :: Testable a => Int -> a -> Bool
holds n = and . take n . results

-- | Check if a property fails up to some value in the enumeration.
fails :: Testable a => Int -> a -> Bool
fails n = not . holds n

-- | Check if there exists an assignment of values that makes the property true.
exists :: Testable a => Int -> a -> Bool
exists n = or . take n . results

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
