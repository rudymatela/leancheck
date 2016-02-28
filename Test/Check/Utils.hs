-- | Utilities functions for manipulating listings (sized lists of lists)
module Test.Check.Utils
  (
  -- * Extra listing constructors
    consFromList
  , consFromStrictlyAscendingList
  , consFromSet
  , consFromNoDupList

  -- * Listing products
  , lsProduct3With
  , lsProductMaybeWith

  -- * Listing lists
  , lsListsOf
  , lsStrictlyAscendingListsOf
  , lsSetsOf
  , lsNoDupListsOf
  , lsProducts
  , listingsOfLength

  -- Deprecated:
  , lsCrescListsOf
  , djs

  -- * Functions

  -- ** Listing
  , lsAssociations
  , lsFunctionPairs
  , functionPairs

  -- ** Pairs to actual functions
  , pairsToMaybeFunction
  , pairsToFunction
  , defaultPairsToFunction
  , defaultFunPairsToFunction
  )
where

import Test.Check.Basic
import Data.Maybe (fromMaybe, catMaybes)

-- | Given a constructor for a type that takes a list, return a listing for
--   that type.
consFromList :: Listable a => ([a] -> b) -> [[b]]
consFromList = (`lsmap` lsListsOf listing)

-- | Given a constructor for a type that takes a list with strictly ascending
--   elements, return a listing for that type (e.g.: a Set type).
consFromStrictlyAscendingList :: Listable a => ([a] -> b) -> [[b]]
consFromStrictlyAscendingList = (`lsmap` lsStrictlyAscendingListsOf listing)

-- | Given a constructor for a type that takes a list with strictly ascending
--   elements, return a listing for that type (e.g.: a Set type).
consFromSet :: Listable a => ([a] -> b) -> [[b]]
consFromSet = consFromStrictlyAscendingList

-- | Given a constructor for a type that takes a list with no duplicate
--   elements, return a listing for that type.
consFromNoDupList :: Listable a => ([a] -> b) -> [[b]]
consFromNoDupList f = lsmap f (lsNoDupListsOf listing)


-- | Like 'lsProduct', but on 3 listings.
lsProduct3With :: (a->b->c->d) -> [[a]] -> [[b]] -> [[c]] -> [[d]]
lsProduct3With f xss yss zss = lsProductWith ($) (lsProductWith f xss yss) zss

lsProductMaybeWith :: (a->b->Maybe c) -> [[a]] -> [[b]] -> [[c]]
lsProductMaybeWith _ _ [] = []
lsProductMaybeWith _ [] _ = []
lsProductMaybeWith f xss (ys:yss) = zs  :  zss \/ lsProductMaybeWith f xss yss
  where (zs:zss) = map (`pwf` ys) xss
        pwf      = productWithMaybe f

productWithMaybe :: (a->b->Maybe c) -> [a] -> [b] -> [c]
productWithMaybe f xs ys = catMaybes $ productWith f xs ys


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

-- | Generates several lists of the same size.
--
-- > lsProducts [ lsX, lsY, lsZ ] ==
--
-- All lists combining elements of listings lsX, lsY and lsZ
lsProducts :: [ [[a]] ] -> [[ [a] ]]
lsProducts = foldr (lsProductWith (:)) [[[]]]

-- | Given a listing of values, returns a listing of lists of no repeated
-- elements.
--
-- > lsNoDupListsOf [[0],[1],[2],...] ==
-- >   [ [[]]
-- >   , [[0]]
-- >   , [[1]]
-- >   , [[0,1],[1,0],[2]]
-- >   , [[0,2],[2,0],[3]]
-- >   , ...
-- >   ]
lsNoDupListsOf :: [[a]] -> [[[a]]]
lsNoDupListsOf = ([[]]:) . lsConcat . djsWith (\x xss -> lsmap (x:) (lsNoDupListsOf xss))

-- | Using a sized list, forms disjoint sized pairs of elements and sized lists
-- of elements.
--
-- > djs [[False,True]] == [[(False,[[True]]),(True,[[False]])]]
-- > djs [[1],[2],[3]]  == [[(1,[[],[2],[3]])],[(2,[[1],[],[3]])],[(3,[[1],[2],[]])]]
--
-- The returned list is sized by the extracted element.
-- This is intended only to be used in 'lsNoDupListsOf'
djs :: [[a]] -> [[(a,[[a]])]]
djs = djsWith (,)

djsWith :: (a -> [[a]] -> b) -> [[a]] -> [[b]]
djsWith f []           = []
djsWith f [[]]         = []
djsWith f ([]:xss)     = [] : djsWith (\y yss -> f y ([]:yss)) xss
djsWith f ((x:xs):xss) = [[f x (xs:xss)]] \/ djsWith (\y (ys:yss) -> f y ((x:ys):yss)) (xs:xss)

-- | Given a listing of values, returns a listing of lists of elements in
--   crescent order (from listing enumeration).  If you only care about wether
--   elements are in returned lists or not, this is the same as listing all the
--   sets.
--
-- > lsStrictlyAscendingListsOf [[0],[1],[2],...] ==
-- >   [ [[]]
-- >   , [[0]]
-- >   , [[1]]
-- >   , [[0,1],[2]]
-- >   , [[0,2],[3]]
-- >   , [[0,3],[1,2],[4]]
-- >   , [[0,1,2],[0,4],[1,3],[5]]
-- >   , ...
-- >   ]
lsStrictlyAscendingListsOf :: [[a]] -> [[[a]]]
lsStrictlyAscendingListsOf = ([[]]:)
                           . lsConcat
                           . ejsWith (\x xss -> lsmap (x:) (lsStrictlyAscendingListsOf xss))

-- | Returns a listing of sets represented as lists of values (no sets are
--   repeated).  Shorthand for 'lsStrictlyAscendingListsOf'.
lsSetsOf :: [[a]] -> [[[a]]]
lsSetsOf = lsStrictlyAscendingListsOf

-- Deprecated name
lsCrescListsOf :: [[a]] -> [[[a]]]
lsCrescListsOf = lsStrictlyAscendingListsOf

-- | Using a sized list, forms crescent sized pairs of elements and sized lists
--   of elements.  This is similar to 'djs' differing only that the elements
--   listed second in the pair are always "greater" (in terms of enumeration) than
--   the first.  Intended only to be used in 'lsCrescListsOf'.
--
-- ejs [[1],[2],[3]]  == [[(1,[[],[2],[3]])],[(2,[[],[],[3]])],[(3,[[],[],[]])]]
-- ejs [[False,True]] == [[(False,[[True]]),(True,[[]])]]
ejs :: [[a]] -> [[(a,[[a]])]]
ejs = ejsWith (,)

ejsWith :: (a -> [[a]] -> b) -> [[a]] -> [[b]]
ejsWith f []           = []
ejsWith f [[]]         = []
ejsWith f ([]:xss)     = [] : ejsWith (\y yss -> f y ([]:yss)) xss
ejsWith f ((x:xs):xss) = [[f x (xs:xss)]] \/ ejsWith f (xs:xss)


-- | Given a listing, returns a listing of lists of a given length.
listingsOfLength :: Int -> [[a]] -> [[[a]]]
listingsOfLength n xss = lsProducts (replicate n xss)


-- | Given a list of values (the domain), and a listing of values (the codomain),
-- return a listing of lists of ordered pairs (associating the domain and codomain).
--
-- This can be viewed as a list of functional left-total relations between values
-- in the domain and codomain.
lsAssociations :: [a] -> [[b]] -> [[ [(a,b)] ]]
lsAssociations xs sbs = zip xs `lsmap` lsProducts (const sbs `map` xs)

-- | Given two listings, list all possible lists of input-output pairs
-- representing functions from values in the first listing
-- to values in the second listing.  Results are returned in lists of
-- increasing size.
lsFunctionPairs :: [[a]] -> [[b]] -> [[[(a,b)]]]
lsFunctionPairs xss yss = lsConcatMap (`lsAssociations` yss)
                                      (lsStrictlyAscendingListsOf xss)


-- | Given two listings, list all possible lists of input-output pairs
-- representing functions from values in the first listing
-- to values in the second listing.
functionPairs :: [[a]] -> [[b]] -> [[(a,b)]]
functionPairs xss = concat . lsFunctionPairs xss


-- | Returns a function given by a list of input-output pairs.
-- The result is wrapped in a maybe value.
-- The output for bound inputs is 'Just' a value.
-- The output for unbound inputs is 'Nothing'.
pairsToMaybeFunction :: Eq a => [(a,b)] -> a -> Maybe b
pairsToMaybeFunction []          _ = Nothing
pairsToMaybeFunction ((a',r):bs) a | a == a'   = Just r
                                   | otherwise = pairsToMaybeFunction bs a

-- | Returns a partial function given by a list of input-output pairs.
--
-- NOTE: This function *will* return undefined values for unbound inputs.
pairsToFunction :: Eq a => [(a,b)] -> a -> b
pairsToFunction bs a = fromMaybe undefined (pairsToMaybeFunction bs a)


-- | Returns a function given by a list of input-output pairs and a default value.
defaultPairsToFunction :: Eq a => b -> [(a,b)] -> a -> b
defaultPairsToFunction r bs a = fromMaybe r (pairsToMaybeFunction bs a)


defaultFunPairsToFunction :: Eq a => (a -> b) -> [(a,b)] -> a -> b
defaultFunPairsToFunction f bs a = fromMaybe (f a) (pairsToMaybeFunction bs a)
