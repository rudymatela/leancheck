module Test.Check.Utils
  ( lsNoDupListsOf
  , lsNoDecListsOf
  , listingsOfLength
  , functionsOn
  , lsPartialFunctions
  , lsFunctions
  , partialFunctions
  , bindingsToFunction'
  , totalBindingsToFunction
  )
where

import Test.Check
import Data.Maybe (fromMaybe)

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
-- crescent order (from listing enumeration)
--
-- > lsNoDecListsOf [[0],[1],[2],...] ==
-- >   [ [[]]
-- >   , [0]
-- >   , [1]
-- >   , [[0,1],[2]]
-- >   , [[0,2],[3]]
-- >   , [[0,3],[1,2],[4]]
-- >   , [[0,1,2],[0,4],[1,3],[5]]
lsNoDecListsOf :: [[a]] -> [[[a]]]
lsNoDecListsOf = ([[]]:) . lsConcat . ejsWith (\x xss -> lsmap (x:) (lsNoDecListsOf xss))

-- | Using a sized list, forms crescent sized pairs of elements and sized lists
--   of elements.  This is similar to 'djs' differing only that the elements
--   listed second in the pair are always "greater" (in terms of enumeration) than
--   the first.  Intended only to be used in 'lsNoDecListsOf'.
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
listingsOfLength n xss = foldr (lsProductWith (:)) [[[]]] (replicate n xss)

functionsOn :: [a] -> [[b]] -> [[[(a,b)]]]
functionsOn xs sbs = lsmap (zip xs) (listingsOfLength (length xs) sbs)


lsPartialFunctions :: [[a]] -> [[b]] -> [[[(a,b)]]]
lsPartialFunctions xss yss = lsConcatMap (`functionsOn` yss) (lsNoDecListsOf xss)


lsFunctions :: [[a]] -> [[b]] -> [[([(a,b)],b)]]
lsFunctions xss yss = lsConcatMap (\(r,yss) -> lsmap (\ps -> (ps,r)) $ lsPartialFunctions xss yss) (djs yss)


partialFunctions :: [[a]] -> [[b]] -> [[(a,b)]]
partialFunctions xss = concat . lsPartialFunctions xss


bindingsToFunction' :: Eq a => [(a,b)] -> a -> Maybe b
bindingsToFunction' []          _ = Nothing
bindingsToFunction' ((a',r):bs) a | a == a'   = Just r
                                  | otherwise = bindingsToFunction' bs a


totalBindingsToFunction :: Eq a => ([(a,b)],b) -> (a -> b)
totalBindingsToFunction (bs,r) a = fromMaybe r (bindingsToFunction' bs a)
