-- | Very simple property-based testing library based on enumeration of values
--   via lists of lists.
module Test.Check
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

  -- ** Unweighed listing constructors
  , cons0
  , cons1
  , cons2
  , cons3
  , cons4
  , cons5
  , cons6
  , cons7
  , cons8
  , cons9
  , cons10
  , cons11
  , cons12

  -- ** Weighed listing constructors
  , wcons0
  , wcons1
  , wcons2
  , wcons3
  , wcons4
  , wcons5
  , wcons6
  , wcons7
  , wcons8
  , wcons9
  , wcons10
  , wcons11
  , wcons12

  -- ** Combining listings
  , (\\//), (\++/), (\+:/)
  , lsListsOf
  , lsProduct
  , lsProductWith
  , lsProduct3With -- might be uneeded
  , lsProducts

  -- ** Manipulating listings
  , lsmap
  , lsfilter
  , lsConcat
  , lsConcatMap

  -- ** Misc utilities
  , (\/)
  , (==>)
  )
where

import Data.Maybe (catMaybes, listToMaybe)

-- | Minimal complete definition: listing or list
class Listable a where
  listing:: [[a]]
  list :: [a]
  listing = map (:[]) list
  list = concat listing


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

(\+:/) :: [[a]] -> [[a]] -> [[a]]
xss \+:/ yss = xss \++/ ([]:yss)

infixr 9 \+:/ -- TODO: remove this (uneeded)

(\\://) :: [[a]] -> [[a]] -> [[a]]
xss \\:// yss = zipWith' (\/) [] [] xss ([]:yss)

-- Is this one is commutative?
-- Can I keep tail, to remove the empty list at the start??
-- How does this apply when building custom instances??
(\\:://) :: [[a]] -> [[a]] -> [[a]]
xss \\::// yss = tail $ zipWith' (\/) [] [] ([]:xss) ([]:yss)

(><) :: [a] -> [b] -> [(a,b)]
[]     ><  _  = []
(x:xs) >< ys = map ((,) x) ys ++ xs >< ys

productWith :: (a->b->c) -> [a] -> [b] -> [c]
productWith _ []     _   =  []
productWith f (x:xs) ys  =  map (f x) ys ++ productWith f xs ys

productWithMaybe :: (a->b->Maybe c) -> [a] -> [b] -> [c]
productWithMaybe f xs ys = catMaybes $ productWith f xs ys

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

-- | Given a listing of values, returns a listing of lists of those values
--
-- > lsListsOf [[]] == [[[]]]
--
-- > lsListsOf [[x]] == [ [[]]
-- >                    , [[x]]
-- >                    , [[x,x]]
-- >                    , [[x,x,x]]
-- >                    , ...
-- >                    ]
--
-- > lsListsOf [[x],[y]] == [ [[]]
-- >                        , [[x]]
-- >                        , [[x,x],[y]]
-- >                        , [[x,x,x],[x,y],[y,x]]
-- >                        , ...
-- >                        ]
lsListsOf :: [[a]] -> [[[a]]]
lsListsOf xss = [[ [] ]] ++ lsProductWith (:) xss (lsListsOf xss)

-- Generates several lists of the same size.
--
-- > lsProducts [ lsX, lsY, lsZ ] ==
--
-- All lists combining elements of series lsX, lsY and lsZ
lsProducts :: [ [[a]] ] -> [[ [a] ]]
lsProducts = foldr (lsProductWith (:)) [[[]]]

lsProductWith :: (a->b->c) -> [[a]] -> [[b]] -> [[c]]
lsProductWith _ _ [] = []
lsProductWith _ [] _ = []
lsProductWith f (xs:xss) yss = zs  :  zss \++/ lsProductWith f xss yss
  where (zs:zss) = map (productWith f xs) yss

lsProduct3With :: (a->b->c->d) -> [[a]] -> [[b]] -> [[c]] -> [[d]]
lsProduct3With f xss yss zss = lsProductWith ($) (lsProductWith f xss yss) zss

lsProductMaybeWith :: (a->b->Maybe c) -> [[a]] -> [[b]] -> [[c]]
lsProductMaybeWith _ _ [] = []
lsProductMaybeWith _ [] _ = []
lsProductMaybeWith f xss (ys:yss) = zs  :  zss \++/ lsProductMaybeWith f xss yss
  where (zs:zss) = map (`pwf` ys) xss
        pwf      = productWithMaybe f

lsConcat :: [[ [[a]] ]] -> [[a]]
lsConcat = foldr (\+:/) [] . map (foldr (\++/) [])

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

(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p
infixr 0 ==>
