-- | Utilities functions for manipulating listings (sized lists of lists)
module Test.Check.Utils
  (
  -- * Extra constructors
    consFromSOrderedList
  , consFromSet
  , consFromNoDupList

  -- * Lists
  , lsNoDupListsOf
  , lsCrescListsOf
  , listingsOfLength
  , djs

  -- * Functions

  -- ** Listing
  , associations
  , associations'
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
import Data.Maybe (fromMaybe)

consFromSOrderedList :: Listable a => ([a] -> b) -> [[b]]
consFromSOrderedList f = lsmap f (lsCrescListsOf listing)

consFromSet :: Listable a => ([a] -> b) -> [[b]]
consFromSet = consFromSOrderedList

consFromNoDupList :: Listable a => ([a] -> b) -> [[b]]
consFromNoDupList f = lsmap f (lsNoDupListsOf listing)

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
djsWith f ((x:xs):xss) = [[f x (xs:xss)]] \++/ djsWith (\y (ys:yss) -> f y ((x:ys):yss)) (xs:xss)

-- | Given a listing of values, returns a listing of lists of elements in
--   crescent order (from listing enumeration).  If you only care about wether
--   elements are in returned lists or not, this is the same as listing all the
--   sets.
--
-- > lsNoDecListsOf [[0],[1],[2],...] ==
-- >   [ [[]]
-- >   , [[0]]
-- >   , [[1]]
-- >   , [[0,1],[2]]
-- >   , [[0,2],[3]]
-- >   , [[0,3],[1,2],[4]]
-- >   , [[0,1,2],[0,4],[1,3],[5]]
-- >   , ...
-- >   ]
lsCrescListsOf :: [[a]] -> [[[a]]]
lsCrescListsOf = ([[]]:) . lsConcat . ejsWith (\x xss -> lsmap (x:) (lsCrescListsOf xss))

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
ejsWith f ((x:xs):xss) = [[f x (xs:xss)]] \++/ ejsWith f (xs:xss)


-- | Given a listing, returns a listing of lists of a given length.
listingsOfLength :: Int -> [[a]] -> [[[a]]]
listingsOfLength n xss = lsProducts (replicate n xss)


-- | Given a list of values (the domain), and a listing of values (the codomain),
-- return a listing of lists of ordered pairs (associating the domain and codomain)
associations :: [a] -> [[b]] -> [[[(a,b)]]]
associations xs sbs = associations' xs (const sbs)

-- | Given a list of values (the domain)
--     and a function to create listing of possible values from a domain value,
-- return a listing of lists of ordered pairs (associating the domain and codomain)
associations' :: [a] -> (a -> [[b]]) -> [[[(a,b)]]]
associations' xs f = lsmap (zip xs) (lsProducts (map f xs))

-- | Given two listings, list all possible lists of input-output pairs
-- representing functions from values in the first listing
-- to values in the second listing.  Results are returned in lists of
-- increasing size.
lsFunctionPairs :: [[a]] -> [[b]] -> [[[(a,b)]]]
lsFunctionPairs xss yss = lsConcatMap (`associations` yss) (lsCrescListsOf xss)


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
