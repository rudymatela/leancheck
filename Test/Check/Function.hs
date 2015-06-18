-- | Function enumeration via lists of pairs.
module Test.Check.Function
where


import Test.Check
import Data.Maybe (fromMaybe)


instance (Eq a, Listable a, Listable b) => Listable (a -> b) where
  listing = lsmap totalBindingsToFunction $ lsFunctions list listing


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


listingsOfLength :: Int -> [[a]] -> [[[a]]]
listingsOfLength n xss = foldr (lsProductWith (:)) [[[]]] (replicate n xss)


functionsOn :: [a] -> [[b]] -> [[[(a,b)]]]
functionsOn as sbs = lsmap (zip as) (listingsOfLength (length as) sbs)


lsPartialFunctions :: [[a]] -> [[b]] -> [[[(a,b)]]]
lsPartialFunctions ass rss = lsConcatMap (`functionsOn` rss) (lsNoDecListsOf ass)


lsFunctions :: [[a]] -> [[b]] -> [[([(a,b)],b)]]
lsFunctions ass rss = lsConcatMap (\(r,rss) -> lsmap (\ps -> (ps,r)) $ lsPartialFunctions ass rss) (djs rss)


partialFunctions :: [[a]] -> [[b]] -> [[(a,b)]]
partialFunctions ass = concat . lsPartialFunctions ass


bindingsToFunction' :: Eq a => [(a,b)] -> a -> Maybe b
bindingsToFunction' []          _ = Nothing
bindingsToFunction' ((a',r):bs) a | a == a'   = Just r
                                  | otherwise = bindingsToFunction' bs a


totalBindingsToFunction :: Eq a => ([(a,b)],b) -> (a -> b)
totalBindingsToFunction (bs,r) a = fromMaybe r (bindingsToFunction' bs a)
