-- | Some invariants over Test.Check functions
--   You should be importing this ONLY to test 'Check.hs' itself.
module Test.Check.Invariants
-- TODO: export things explictly
where

import Test.Check
import Data.List
import Data.Ord

-- For use in testing.
newtype Nat = N { unN :: Int }
  deriving (Eq, Ord)

-- For this module, 0 is a Natural number: I'm super cereal.
instance Listable Nat where
  list = map N [0,1..]

instance Show Nat where
  show (N x) = show x

-- | check if a list is ordered
ordered :: Ord a => [a] -> Bool
ordered = orderedBy compare
-- ordered [] = True
-- ordered [_] = True
-- ordered (x:y:xs) = x <= y && ordered (y:xs)

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

thn :: (a->a->Ordering) -> (a->a->Ordering) -> (a->a->Ordering)
thn cmp1 cmp2 x y = (x `cmp1` y) `ifNotEq` (x `cmp2` y)
infixr 9 `thn`


-- | checks if the first 'n' elements of the listing are ordered by 'cmp'.
--
-- > (n `seriesOrderedBy`) comparing (id :: Type)
lsOrderedBy :: Listable a => Int -> (a -> a -> Ordering) -> Bool
lsOrderedBy n cmp = orderedBy cmp $ take n list
infixr 9 `lsOrderedBy`

lsStrictlyOrderedBy :: Listable a => Int -> (a -> a -> Ordering) -> Bool
lsStrictlyOrderedBy n cmp = strictlyOrderedBy cmp $ take n list
infixr 9 `lsStrictlyOrderedBy`

lsNatPairOrd :: Int -> Bool
lsNatPairOrd n = n `lsStrictlyOrderedBy`  comparing sum' `thn` flip compare
  where sum' (N x,N y) = x+y

lsNatTripleOrd :: Int -> Bool
lsNatTripleOrd n = n `lsStrictlyOrderedBy`  comparing sum' `thn` flip compare
  where sum' (N x,N y,N z) = x+y+z

lsNatQuadrupleOrd :: Int -> Bool
lsNatQuadrupleOrd n = n `lsStrictlyOrderedBy`  comparing sum' `thn` flip compare
  where sum' (N x,N y,N z,N w) = x+y+z+w

lsNatQuintupleOrd :: Int -> Bool
lsNatQuintupleOrd n = n `lsStrictlyOrderedBy`  comparing sum' `thn` flip compare
  where sum' (N x,N y,N z,N w,N v) = x+y+z+w+v

lsNatListOrd :: Int -> Bool
lsNatListOrd n = n `lsStrictlyOrderedBy`  comparing sum' `thn` flip compare
  where sum' = sum . map ((+1) . unN)

lsPairEqParams :: Int -> Bool
lsPairEqParams n = ces == srs
  where
    ces = map (map read) $ counterExamples n fail
    srs = map (pairToList) $ take n $ list
    pairToList (x,y) = map unN [y,x]
    fail :: Nat -> Nat -> Bool
    fail x y = False

lsTripleEqParams :: Int -> Bool
lsTripleEqParams n = ces == srs
  where
    ces = map (map read) $ counterExamples n fail
    srs = map tripleToList $ take n $ list
    tripleToList (x,y,z) = map unN [z,y,x]
    fail :: Nat -> Nat -> Nat -> Bool
    fail x y z = False

