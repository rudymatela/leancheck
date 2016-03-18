-- | Some invariants over Test.Check functions
--   You should be importing this ONLY to test 'Check.hs' itself.
module Test.Check.Invariants
  ( tNatPairOrd
  , tNatTripleOrd
  , tNatQuadrupleOrd
  , tNatQuintupleOrd
  , tNatSixtupleOrd
  , tNatListOrd
  , tListsOfNatOrd
  , tPairEqParams
  , tTripleEqParams
  , tProductsIsFilterByLength

  , ordered
  , orderedBy
  , strictlyOrdered
  , strictlyOrderedBy
  )
where

import Test.Check
import Data.List
import Data.Ord
import Test.Types (Nat(..))

-- | check if a list is ordered
ordered :: Ord a => [a] -> Bool
ordered = orderedBy compare
-- ordered [] = True
-- ordered [_] = True
-- ordered (x:y:xs) = x <= y && ordered (y:xs)

strictlyOrdered :: Ord a => [a] -> Bool
strictlyOrdered = strictlyOrderedBy compare

-- | check if a list is ordered by a given ordering function
orderedBy :: (a -> a -> Ordering) -> [a] -> Bool
orderedBy _ [] = True
orderedBy _ [_] = True
orderedBy cmp (x:y:xs) = case x `cmp` y of
                           GT -> False
                           _  -> orderedBy cmp (y:xs)

-- | check if a list is strictly ordered by a given ordering function
strictlyOrderedBy :: (a -> a -> Ordering) -> [a] -> Bool
strictlyOrderedBy _ [] = True
strictlyOrderedBy _ [_] = True
strictlyOrderedBy cmp (x:y:xs) = case x `cmp` y of
                                   LT -> strictlyOrderedBy cmp (y:xs)
                                   _  -> False

ifNotEq :: Ordering -> Ordering -> Ordering
-- Could be implemented as:  ifNotEq = mappend
ifNotEq EQ p = p
ifNotEq  o _ = o

thn :: (a->a->Ordering) -> (a->a->Ordering) -> a -> a -> Ordering
thn cmp1 cmp2 x y = (x `cmp1` y) `ifNotEq` (x `cmp2` y)
infixr 9 `thn`


-- | checks if the first 'n' elements on tiers are ordered by 'cmp'.
--
-- > (n `seriesOrderedBy`) comparing (id :: Type)
tOrderedBy :: Listable a => Int -> (a -> a -> Ordering) -> Bool
tOrderedBy n cmp = orderedBy cmp $ take n list
infixr 9 `tOrderedBy`

tStrictlyOrderedBy :: Listable a => Int -> (a -> a -> Ordering) -> Bool
tStrictlyOrderedBy n cmp = strictlyOrderedBy cmp $ take n list
infixr 9 `tStrictlyOrderedBy`

tNatPairOrd :: Int -> Bool
tNatPairOrd n = n `tStrictlyOrderedBy`  comparing sum' `thn` compare
  where sum' (x,y) = x+y :: Nat

tNatTripleOrd :: Int -> Bool
tNatTripleOrd n = n `tStrictlyOrderedBy`  comparing sum' `thn` compare
  where sum' (x,y,z) = x+y+z :: Nat

tNatQuadrupleOrd :: Int -> Bool
tNatQuadrupleOrd n = n `tStrictlyOrderedBy`  comparing sum' `thn` compare
  where sum' (x,y,z,w) = x+y+z+w :: Nat

tNatQuintupleOrd :: Int -> Bool
tNatQuintupleOrd n = n `tStrictlyOrderedBy`  comparing sum' `thn` compare
  where sum' (x,y,z,w,v) = x+y+z+w+v :: Nat

tNatSixtupleOrd :: Int -> Bool
tNatSixtupleOrd n = n `tStrictlyOrderedBy`  comparing sum' `thn` compare
  where sum' (x,y,z,w,v,u) = x+y+z+w+v+u :: Nat

tNatListOrd :: Int -> Bool
tNatListOrd n = n `tStrictlyOrderedBy`  comparing sum' `thn` compare
  where sum' = sum . map (+1) :: [Nat] -> Nat

tListsOfStrictlyOrderedBy :: Int
                           -> (a -> a -> Ordering)
                           -> [[a]]
                           -> Bool
tListsOfStrictlyOrderedBy n cmp = strictlyOrderedBy cmp . take n . concat
infixr 9 `tListsOfStrictlyOrderedBy`

tListsOfNatOrd :: Int -> Bool
tListsOfNatOrd n = tListsOfStrictlyOrderedBy n (comparing sum' `thn` compare) tiers
  where sum' = sum . map (+1) :: [Nat] -> Nat

tPairEqParams :: Int -> Bool
tPairEqParams n = ces == srs
  where
    ces = map (map read) $ counterExamples n fail
    srs = map pairToList $ take n list
    pairToList (x,y) = [x,y :: Nat]
    fail :: Nat -> Nat -> Bool
    fail x y = False

tTripleEqParams :: Int -> Bool
tTripleEqParams n = ces == srs
  where
    ces = map (map read) $ counterExamples n fail
    srs = map tripleToList $ take n list
    tripleToList (x,y,z) = [x,y,z :: Nat]
    fail :: Nat -> Nat -> Nat -> Bool
    fail x y z = False

tProductsIsFilterByLength :: Eq a => [[a]] -> Int -> Int -> Bool
tProductsIsFilterByLength values m n = concat (take m byProduct) `isPrefixOf` concat byFilter
  where byProduct = products $ replicate n values
        byFilter  = ((==n) . length) `filterT` listsOf values
