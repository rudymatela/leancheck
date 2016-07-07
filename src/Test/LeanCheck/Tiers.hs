-- | LeanCheck is a simple enumerative property-based testing library.
--
-- This module provides advanced functions for manipulating 'tiers'.
-- Most definitions given here are exported by "Test.LeanCheck", except:
--   'listCons',
--   'choices',
--   'setChoices' and
--   'bagChoices'.
module Test.LeanCheck.Tiers
  (
  -- * Additional tiers constructors
    listCons
  , setCons
  , bagCons
  , noDupListCons

  -- * Products of tiers
  , product3
  , product3With
  , productMaybeWith

  -- * Tiers of lists
  , listsOf
  , bagsOf
  , setsOf
  , noDupListsOf
  , products
  , listsOfLength

  -- * Tiers of pairs
  , distinctPairs
  , distinctPairsWith
  , unorderedPairs
  , unorderedPairsWith
  , unorderedDistinctPairs
  , unorderedDistinctPairsWith

  , deleteT
  , normalizeT

  -- * Tiers of choices
  , choices
  , setChoices
  , bagChoices
  )
where

import Test.LeanCheck.Basic
import Data.Maybe (catMaybes)

-- | Given a constructor that takes a list,
--   return tiers of applications of this constructor.
--
-- This is basically a type-restricted version of 'cons1'.
-- You should use 'cons1' instead: this serves more as an illustration of how
-- 'setCons' and 'bagCons' work (see source).
listCons :: Listable a => ([a] -> b) -> [[b]]
listCons = (`mapT` listsOf tiers)

-- | Given a constructor that takes a bag of elements (as a list),
--   lists tiers of applications of this constructor.
--
-- For example, a 'Bag' represented as a list.
--
-- > bagCons Bag
bagCons :: Listable a => ([a] -> b) -> [[b]]
bagCons = (`mapT` bagsOf tiers)

-- | Given a constructor that takes a set of elements (as a list),
--   lists tiers of applications of this constructor.
--
-- A naive 'Listable' instance for the 'Data.Set.Set' (of "Data.Set")
-- would read:
--
-- > instance Listable a => Listable (Set a) where
-- >   tiers = cons0 empty \/ cons2 insert
--
-- The above instance has a problem: it generates repeated sets.
-- A more efficient implementation that does not repeat sets is given by:
--
-- >   tiers = setCons fromList
--
-- Alternatively, you can use 'setsOf' direclty.
setCons :: Listable a => ([a] -> b) -> [[b]]
setCons = (`mapT` setsOf tiers)

-- | Given a constructor that takes a list with no duplicate elements,
--   return tiers of applications of this constructor.
noDupListCons :: Listable a => ([a] -> b) -> [[b]]
noDupListCons f = mapT f (noDupListsOf tiers)

-- | Like '><', but over 3 lists of tiers.
product3 :: [[a]] -> [[b]]-> [[c]] -> [[(a,b,c)]]
product3 = product3With (\x y z -> (x,y,z))

-- | Like 'productWith', but over 3 lists of tiers.
product3With :: (a->b->c->d) -> [[a]] -> [[b]] -> [[c]] -> [[d]]
product3With f xss yss zss = productWith ($) (productWith f xss yss) zss

-- | Take the product of lists of tiers
--   by a function returning a 'Maybe' value
--   discarding 'Nothing' values.
productMaybeWith :: (a->b->Maybe c) -> [[a]] -> [[b]] -> [[c]]
productMaybeWith _ _ [] = []
productMaybeWith _ [] _ = []
productMaybeWith f (xs:xss) yss = map (xs **) yss
                               \/ productMaybeWith f xss yss `addWeight` 1
  where xs ** ys = catMaybes [ f x y | x <- xs, y <- ys ]

-- | Takes as argument tiers of element values;
--   returns tiers of pairs with distinct element values.
--
-- When argument tiers have no repeated elements:
--
-- > distinctPairs xss  =  xss >< xss  `suchThat` uncurry (/=)
distinctPairs :: [[a]] -> [[(a,a)]]
distinctPairs = distinctPairsWith (,)

-- | 'distinctPairs' by a given function:
--
-- > distinctPairsWith f = mapT (uncurry f) . distinctPairs
distinctPairsWith :: (a -> a -> b) -> [[a]] -> [[b]]
distinctPairsWith f = concatT . choicesWith (\e -> mapT (f e))

-- | Takes as argument tiers of element values;
--   returns tiers of unordered pairs where, in enumeration order,
--   the first element is less than or equal to the second.
--
-- The name of this function is perhaps a misnomer.  But in mathematics,
-- an unordered pair is a pair where you don't care about element order, e.g.:
-- @(1,2) = (2,1)@.  This function will enumerate canonical versions of such
-- pairs where the first element is less than the second.
--
-- The returned element pairs can be seen as bags with two elements.
--
-- When argument tiers are listed in 'Ord':
--
-- > distinctPairs xss  =  xss >< xss  `suchThat` uncurry (<=)
unorderedPairs :: [[a]] -> [[(a,a)]]
unorderedPairs = unorderedPairsWith (,)

-- | 'unorderedPairs' by a given function:
--
-- > unorderedPairsWith f = mapT (uncurry f) . unorderedPairs
unorderedPairsWith :: (a -> a -> b) -> [[a]] -> [[b]]
unorderedPairsWith f = concatT . bagChoicesWith (\e -> mapT (f e))

-- | Takes as argument tiers of element values;
--   returns tiers of unordered pairs where, in enumeration order,
--   the first element is strictly less than the second.
--
-- The returned element pairs can be seen as sets with two elements.
--
-- When argument tiers are listed in 'Ord':
--
-- > distinctPairs xss  =  xss >< xss  `suchThat` uncurry (<)
unorderedDistinctPairs :: [[a]] -> [[(a,a)]]
unorderedDistinctPairs = unorderedDistinctPairsWith (,)

-- | 'unorderedPairs' by a given function:
--
-- > unorderedDistinctPairsWith f = mapT (uncurry f) . unorderedDistinctPairs
unorderedDistinctPairsWith :: (a -> a -> b) -> [[a]] -> [[b]]
unorderedDistinctPairsWith f = concatT . setChoicesWith (\e -> mapT (f e))

-- | Takes as argument tiers of element values;
--   returns tiers of lists of elements.
--
-- > listsOf [[]] == [[[]]]
--
-- > listsOf [[x]] == [ [[]]
-- >                  , [[x]]
-- >                  , [[x,x]]
-- >                  , [[x,x,x]]
-- >                  , ...
-- >                  ]
--
-- > listsOf [[x],[y]] == [ [[]]
-- >                      , [[x]]
-- >                      , [[x,x],[y]]
-- >                      , [[x,x,x],[x,y],[y,x]]
-- >                      , ...
-- >                      ]
listsOf :: [[a]] -> [[[a]]]
listsOf xss = cons0 []
           \/ productWith (:) xss (listsOf xss) `addWeight` 1

-- | Takes the product of N lists of tiers, producing lists of length N.
--
-- Alternatively,
-- takes as argument a list of lists of tiers of elements;
-- returns lists combining elements of each list of tiers.
--
-- > products [xss] = mapT (:[]) xss
-- > products [xss,yss] = mapT (\(x,y) -> [x,y]) (xss >< yss)
-- > products [xss,yss,zss] = product3With (\x y z -> [x,y,z]) xss yss zss
products :: [ [[a]] ] -> [[ [a] ]]
products = foldr (productWith (:)) [[[]]]

-- | Delete the first occurence of an element in a tier.
--
-- For tiers without repetitions, the following holds:
--
-- > deleteT x = normalizeT . (`suchThat` (/= x))
deleteT :: Eq a => a -> [[a]] -> [[a]]
deleteT _ [] = []
deleteT y ([]:xss) = [] : deleteT y xss
deleteT y [[x]]        | x == y    = []
deleteT y ((x:xs):xss) | x == y    = xs:xss
                       | otherwise = [[x]] \/ deleteT y (xs:xss)

-- | Normalizes tiers by removing an empty tier from the end of a list of
--   tiers.
--
-- > normalizeT [xs0,xs1,...,xsN,[]] = [xs0,xs1,...,xsN]
--
--   Note this will only remove a single empty tier:
--
-- > normalizeT [xs0,xs1,...,xsN,[],[]] = [xs0,xs1,...,xsN,[]]
normalizeT :: [[a]] -> [[a]]
normalizeT [] = []
normalizeT [[]] = []
normalizeT (xs:xss) = xs:normalizeT xss

-- | Takes as argument tiers of element values;
--   returns tiers of lists with no repeated elements.
--
-- > noDupListsOf [[0],[1],[2],...] ==
-- >   [ [[]]
-- >   , [[0]]
-- >   , [[1]]
-- >   , [[0,1],[1,0],[2]]
-- >   , [[0,2],[2,0],[3]]
-- >   , ...
-- >   ]
noDupListsOf :: [[a]] -> [[[a]]]
noDupListsOf =
  ([[]]:) . concatT . choicesWith (\x xss -> mapT (x:) (noDupListsOf xss))

-- | Takes as argument tiers of element values;
--   returns tiers of size-ordered lists of elements possibly with repetition.
--
-- > bagsOf [[0],[1],[2],...] =
-- >   [ [[]]
-- >   , [[0]]
-- >   , [[0,0],[1]]
-- >   , [[0,0,0],[0,1],[2]]
-- >   , [[0,0,0,0],[0,0,1],[0,2],[1,1],[3]]
-- >   , [[0,0,0,0,0],[0,0,0,1],[0,0,2],[0,1,1],[0,3],[1,2],[4]]
-- >   , ...
-- >   ]
bagsOf :: [[a]] -> [[[a]]]
bagsOf = ([[]]:) . concatT . bagChoicesWith (\x xss -> mapT (x:) (bagsOf xss))


-- | Takes as argument tiers of element values;
--   returns tiers of size-ordered lists of elements without repetition.
--
-- > setsOf [[0],[1],[2],...] =
-- >   [ [[]]
-- >   , [[0]]
-- >   , [[1]]
-- >   , [[0,1],[2]]
-- >   , [[0,2],[3]]
-- >   , [[0,3],[1,2],[4]]
-- >   , [[0,1,2],[0,4],[1,3],[5]]
-- >   , ...
-- >   ]
--
-- Can be used in the constructor of specialized 'Listable' instances.
-- For 'Data.Set.Set' (from "Data.Set"), we would have:
--
-- > instance Listable a => Listable (Set a) where
-- >   tiers = mapT fromList $ setsOf tiers
setsOf :: [[a]] -> [[[a]]]
setsOf = ([[]]:) . concatT . setChoicesWith (\x xss -> mapT (x:) (setsOf xss))

-- | Lists tiers of choices.
-- Choices are pairs of values and tiers excluding that value.
--
-- > choices [[False,True]] == [[(False,[[True]]),(True,[[False]])]]
-- > choices [[1],[2],[3]]
-- >   == [ [(1,[[],[2],[3]])]
-- >      , [(2,[[1],[],[3]])]
-- >      , [(3,[[1],[2],[]])] ]
--
-- Each choice is sized by the extracted element.
choices :: [[a]] -> [[(a,[[a]])]]
choices = choicesWith (,)

-- | Like 'choices', but allows a custom function.
choicesWith :: (a -> [[a]] -> b) -> [[a]] -> [[b]]
choicesWith f []           = []
choicesWith f [[]]         = []
choicesWith f ([]:xss)     = [] : choicesWith (\y yss -> f y ([]:yss)) xss
choicesWith f ((x:xs):xss) = [[f x (xs:xss)]]
                          \/ choicesWith (\y (ys:yss) -> f y ((x:ys):yss)) (xs:xss)

-- | Like 'choices' but lists tiers of non-decreasing (ascending) choices.
--   Used to construct 'bagsOf' values.
--
-- > bagChoices [[False,True]] =
-- >   [ [(False,[[False,True]]), (True,[[True]])]
-- >   ]
--
-- > bagChoices [[1],[2],[3],...] =
-- >   [ [(1,[[1],[2],[3],...])]
-- >   , [(2,[[ ],[2],[3],...])]
-- >   , [(3,[[ ],[ ],[3],...])]
-- >   , ...
-- >   ]
bagChoices :: [[a]] -> [[(a,[[a]])]]
bagChoices = bagChoicesWith (,)

-- | Like 'bagChoices' but customized by a function.
bagChoicesWith :: (a -> [[a]] -> b) -> [[a]] -> [[b]]
bagChoicesWith f []           = []
bagChoicesWith f [[]]         = []
bagChoicesWith f ([]:xss)     = [] : bagChoicesWith (\y yss -> f y ([]:yss)) xss
bagChoicesWith f ((x:xs):xss) = [[f x ((x:xs):xss)]]
                             \/ bagChoicesWith f (xs:xss)

-- | Like 'choices' but lists tiers of strictly ascending choices.
--   Used to construct 'setsOf' values.
--
-- > setChoices [[False,True]] == [[(False,[[True]]),(True,[[]])]]
-- > setChoices [[1],[2],[3]]
-- >   == [ [(1,[[],[2],[3]])]
-- >      , [(2,[[],[],[3]])]
-- >      , [(3,[[],[],[]])]
-- >      ]
setChoices :: [[a]] -> [[(a,[[a]])]]
setChoices = setChoicesWith (,)

-- | Like 'setChoices' but customized by a function.
setChoicesWith :: (a -> [[a]] -> b) -> [[a]] -> [[b]]
setChoicesWith f []           = []
setChoicesWith f [[]]         = []
setChoicesWith f ([]:xss)     = [] : setChoicesWith (\y yss -> f y ([]:yss)) xss
setChoicesWith f ((x:xs):xss) = [[f x (xs:xss)]]
                             \/ setChoicesWith f (xs:xss)

-- | Takes as argument an integer length and tiers of element values;
--   returns tiers of lists of element values of the given length.
--
-- > listsOfLength 3 [[0],[1],[2],[3],[4]...] =
-- >   [ [[0,0,0]]
-- >   , [[0,0,1],[0,1,0],[1,0,0]]
-- >   , [[0,0,2],[0,1,1],[0,2,0],[1,0,1],[1,1,0],[2,0,0]]
-- >   , ...
-- >   ]
listsOfLength :: Int -> [[a]] -> [[[a]]]
listsOfLength n xss = products (replicate n xss)
