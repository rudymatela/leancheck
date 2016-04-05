-- | Utilities functions for manipulating tiers (sized lists of lists)
module Test.Check.Utils
  (
  -- * Additional tiers constructors
    consFromList
  , consFromAscendingList
  , consFromStrictlyAscendingList
  , consFromSet
  , consFromNoDupList

  -- * Products of tiers
  , product3With
  , productMaybeWith

  -- * Tiers of lists
  , listsOf
  , ascendingListsOf
  , strictlyAscendingListsOf
  , setsOf
  , noDupListsOf
  , products
  , listsOfLength

  , deleteT
  , normalizeT

  -- * Tiers of choices
  , choices
  , ascendingChoices
  , strictlyAscendingChoices

  -- * Functions

  -- ** Listing
  , associations
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

-- | Given a constructor for a type that takes a list,
--   return tiers for that type.
consFromList :: Listable a => ([a] -> b) -> [[b]]
consFromList = (`mapT` listsOf tiers)

consFromAscendingList :: Listable a => ([a] -> b) -> [[b]]
consFromAscendingList = (`mapT` ascendingListsOf tiers)

-- | Given a constructor for a type that takes a list with strictly ascending
--   elements, return tiers of that type (e.g.: a Set type).
consFromStrictlyAscendingList :: Listable a => ([a] -> b) -> [[b]]
consFromStrictlyAscendingList = (`mapT` strictlyAscendingListsOf tiers)

-- | Given a constructor for a type that takes a set of elements (as a list)
--   return tiers of that type (e.g.: a Set type).
consFromSet :: Listable a => ([a] -> b) -> [[b]]
consFromSet = (`mapT` setsOf tiers)

-- | Given a constructor for a type that takes a list with no duplicate
--   elements, return tiers of that type.
consFromNoDupList :: Listable a => ([a] -> b) -> [[b]]
consFromNoDupList f = mapT f (noDupListsOf tiers)


-- | Like 'product', but over 3 lists of tiers.
product3With :: (a->b->c->d) -> [[a]] -> [[b]] -> [[c]] -> [[d]]
product3With f xss yss zss = productWith ($) (productWith f xss yss) zss

-- | Take the product of lists of tiers by a function returning a maybe value.
productMaybeWith :: (a->b->Maybe c) -> [[a]] -> [[b]] -> [[c]]
productMaybeWith _ _ [] = []
productMaybeWith _ [] _ = []
productMaybeWith f (xs:xss) yss = map (xs **) yss
                               \/ productMaybeWith f xss yss `addWeight` 1
  where xs ** ys = catMaybes [ f x y | x <- xs, y <- ys ]


-- | Given tiers of values, returns tiers of lists of those values
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

-- | Generates several lists of the same size.
--
-- > products [ xss, yss, zss ] ==
--
-- Tiers of all lists combining elements of tiers: xss, yss and zss
products :: [ [[a]] ] -> [[ [a] ]]
products = foldr (productWith (:)) [[[]]]

-- | Delete the first occurence of an element in a tier,
--   for tiers without repetitions:
--
-- > deleteT x === normalizeT . (`suchThat` (/= x))
deleteT :: Eq a => a -> [[a]] -> [[a]]
deleteT _ [] = []
deleteT y ([]:xss) = [] : deleteT y xss
deleteT y [[x]]        | x == y    = []
deleteT y ((x:xs):xss) | x == y    = xs:xss
                       | otherwise = [[x]] \/ deleteT y (xs:xss)

normalizeT :: [[a]] -> [[a]]
normalizeT [] = []
normalizeT [[]] = []
normalizeT (xs:xss) = xs:normalizeT xss

-- | Given tiers of values, returns tiers of lists with no repeated elements.
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

-- | Lists tiers of all choices of values from tiers.
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

-- | Given tiers of values,
--   returns tiers of lists of elements in ascending order
--                               (from tiered enumeration).
--
ascendingListsOf :: [[a]] -> [[[a]]]
ascendingListsOf =
  ([[]]:) . concatT . ascendingChoicesWith (\x xss -> mapT (x:) (ascendingListsOf xss))

-- > ascendingChoices [[False,True]] =
-- >   [ [(False,[[False,True]]), (True,[[True]])]
-- >   ]
--
-- > ascendingChoices [[1],[2],[3],...] =
-- >   [ [(1,[[1],[2],[3],...])]
-- >   , [(2,[[ ],[2],[3],...])]
-- >   , [(3,[[ ],[ ],[3],...])]
-- >   , ...
-- >   ]
ascendingChoices :: [[a]] -> [[(a,[[a]])]]
ascendingChoices = ascendingChoicesWith (,)

ascendingChoicesWith :: (a -> [[a]] -> b) -> [[a]] -> [[b]]
ascendingChoicesWith f []           = []
ascendingChoicesWith f [[]]         = []
ascendingChoicesWith f ([]:xss)     = [] : ascendingChoicesWith (\y yss -> f y ([]:yss)) xss
ascendingChoicesWith f ((x:xs):xss) = [[f x ((x:xs):xss)]]
                                   \/ ascendingChoicesWith f (xs:xss)

-- | Given tiers of values,
--   returns tiers of lists of elements in strictly ascending order
--                              (from tiered enumeration).
--   If you only care about whether elements are in returned lists,
--   this returns the tiers of all sets of values.
--
-- > strictlyAscendingListsOf [[0],[1],[2],...] ==
-- >   [ [[]]
-- >   , [[0]]
-- >   , [[1]]
-- >   , [[0,1],[2]]
-- >   , [[0,2],[3]]
-- >   , [[0,3],[1,2],[4]]
-- >   , [[0,1,2],[0,4],[1,3],[5]]
-- >   , ...
-- >   ]
strictlyAscendingListsOf :: [[a]] -> [[[a]]]
strictlyAscendingListsOf =
  ([[]]:) . concatT .
  strictlyAscendingChoicesWith
    (\x xss -> mapT (x:) (strictlyAscendingListsOf xss))

-- | Returns tiers of sets represented as lists of values (no repeated sets).
--   Shorthand for 'strictlyAscendingListsOf'.
setsOf :: [[a]] -> [[[a]]]
setsOf = strictlyAscendingListsOf

-- | Like 'choices', but paired tiers are always strictly ascending (in terms
--   of enumeration).
--
-- > strictlyAscendingChoices [[False,True]] == [[(False,[[True]]),(True,[[]])]]
-- > strictlyAscendingChoices [[1],[2],[3]]
-- >   == [ [(1,[[],[2],[3]])]
-- >      , [(2,[[],[],[3]])]
-- >      , [(3,[[],[],[]])]
-- >      ]
strictlyAscendingChoices :: [[a]] -> [[(a,[[a]])]]
strictlyAscendingChoices = strictlyAscendingChoicesWith (,)

-- | Like 'strictlyAscendingChoices' but customized by a function.
strictlyAscendingChoicesWith :: (a -> [[a]] -> b) -> [[a]] -> [[b]]
strictlyAscendingChoicesWith f []           = []
strictlyAscendingChoicesWith f [[]]         = []
strictlyAscendingChoicesWith f ([]:xss)     = [] : strictlyAscendingChoicesWith (\y yss -> f y ([]:yss)) xss
strictlyAscendingChoicesWith f ((x:xs):xss) = [[f x (xs:xss)]]
                                           \/ strictlyAscendingChoicesWith f (xs:xss)


-- | Given tiers, returns tiers of lists of a given length.
listsOfLength :: Int -> [[a]] -> [[[a]]]
listsOfLength n xss = products (replicate n xss)


-- | Given a list of domain values, and tiers of codomain values,
-- return tiers of lists of ordered pairs of domain and codomain values.
--
-- Technically: tiers of left-total functional relations.
associations :: [a] -> [[b]] -> [[ [(a,b)] ]]
associations xs sbs = zip xs `mapT` products (const sbs `map` xs)

-- | Given tiers of input values and tiers of output values,
-- return tiers with all possible lists of input-output pairs.
-- Those represent functional relations.
functionPairs :: [[a]] -> [[b]] -> [[[(a,b)]]]
functionPairs xss yss = concatMapT (`associations` yss)
                                   (strictlyAscendingListsOf xss)

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
