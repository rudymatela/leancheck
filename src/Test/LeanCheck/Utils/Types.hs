-- |
-- Module      : Test.LeanCheck.Utils.Types
-- Copyright   : (c) 2015-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- Types to aid in property-based testing.
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ == 708
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
module Test.LeanCheck.Utils.Types
  (
  -- * Integer types
  --
  -- | Small-width integer types to aid in property-based testing.
  -- Sometimes it is useful to limit the possibilities of enumerated values
  -- when testing polymorphic functions, these types allow that.
  --
  -- The signed integer types @IntN@ are of limited bit width @N@
  -- bounded by @-2^(N-1)@ to @2^(N-1)-1@.
  -- The unsigned integer types @WordN@ are of limited bit width @N@
  -- bounded by @0@ to @2^N-1@.
  --
  -- Operations are closed and modulo @2^N@.  e.g.:
  --
  -- > maxBound + 1      = minBound
  -- > read "2"          = -2 :: Int2
  -- > abs minBound      = minBound
  -- > negate n          = 2^N - n :: WordN
    Int1 (..)
  , Int2 (..)
  , Int3 (..)
  , Int4 (..)
  , Word1 (..)
  , Word2 (..)
  , Word3 (..)
  , Word4 (..)
  , Nat (..)
  , Nat1 (..)
  , Nat2 (..)
  , Nat3 (..)
  , Nat4 (..)
  , Nat5 (..)
  , Nat6 (..)
  , Nat7 (..)
  , Natural (..)

  -- ** Aliases to word types (deprecated)
  , UInt1
  , UInt2
  , UInt3
  , UInt4

  -- * Extreme Integers
  , X (..)
  , Xs (..)

  -- * List-wrapper types
  , NoDup (..)
  , Bag (..)
  , Set (..)
  , Map (..)

  -- * Character types
  , Space (..)
  , Lower (..)
  , Upper (..)
  , Alpha (..)
  , Digit (..)
  , AlphaNum (..)
  , Letter (..)

  -- * String types
  , Spaces (..)
  , Lowers (..)
  , Uppers (..)
  , Alphas (..)
  , Digits (..)
  , AlphaNums (..)
  , Letters (..)

  -- * Generic types
  , A, B, C, D, E, F
  )
where

import Test.LeanCheck (Listable(..), listIntegral)
import Test.LeanCheck.Core ((+|),cons1)
import Test.LeanCheck.Tiers (noDupListCons, setCons, bagCons, mapCons)
import Data.Ratio ((%))
import Data.Ix
#if __GLASGOW_HASKELL__ == 708
import Data.Typeable (Typeable)
#endif

narrowU :: Int -> Int -> Int
narrowU w i  =  i `mod` 2^w

narrowS :: Int -> Int -> Int
narrowS w i  =  if i' < 2^(w-1)
                then i'
                else i' - l
                where
                l   =  2^w
                i'  =  i `mod` l


mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (x,y)  =  (f x, f y)

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (x,y)  =  (f x, y)

oNewtype :: (a -> b) -> (b -> a) -> (a -> a -> a) -> (b -> b -> b)
oNewtype con des o  =  \x y -> con $ des x `o` des y

fNewtype :: (a -> b) -> (b -> a) -> (a -> a) -> (b -> b)
fNewtype con des f  =  con . f . des

otNewtype :: (a -> b) -> (b -> a) -> (a -> a -> (a,a)) -> (b -> b -> (b,b))
otNewtype con des o  =  \x y -> mapTuple con $ des x `o` des y

readsPrecNewtype :: Read a => (a -> b) -> Int -> String -> [(b,String)]
readsPrecNewtype con n  =  map (mapFst con) . readsPrec n

boundedEnumFrom :: (Ord a,Bounded a,Enum a) => a -> [a]
boundedEnumFrom x  =  [x..maxBound]

boundedEnumFromThen :: (Ord a,Bounded a,Enum a) => a -> a -> [a]
boundedEnumFromThen x y  | x > y      =  [x,y..minBound]
                         | otherwise  =  [x,y..maxBound]

-- | Single-bit signed integers: -1, 0
newtype Int1  =  Int1 { unInt1 :: Int }  deriving (Eq, Ord)

-- | Two-bit signed integers: -2, -1, 0, 1
newtype Int2  =  Int2 { unInt2 :: Int }  deriving (Eq, Ord)

-- | Three-bit signed integers: -4, -3, -2, -1, 0, 1, 2, 3
newtype Int3  =  Int3 { unInt3 :: Int }  deriving  (Eq, Ord)

-- | Four-bit signed integers:
-- -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7
newtype Int4  =  Int4 { unInt4 :: Int }  deriving (Eq, Ord)

-- | Single-bit unsigned integer: 0, 1
newtype Word1  =  Word1 { unWord1 :: Int }  deriving (Eq, Ord)

-- | Two-bit unsigned integers: 0, 1, 2, 3
newtype Word2  =  Word2 { unWord2 :: Int }  deriving (Eq, Ord)

-- | Three-bit unsigned integers: 0, 1, 2, 3, 4, 5, 6, 7
newtype Word3  =  Word3 { unWord3 :: Int }  deriving (Eq, Ord)

-- | Four-bit unsigned integers:
-- 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
newtype Word4  =  Word4 { unWord4 :: Int }  deriving (Eq, Ord)

-- | Natural numbers (including 0): 0, 1, 2, 3, 4, 5, 6, 7, ...
--
-- Internally, this type is represented as an 'Integer'
-- allowing for an infinity of possible values.
--
-- Its 'Enum', 'Listable' and 'Num' instances only produce non-negative values.
-- When @x < y@ then @x - y = 0@.
newtype Natural  =  Natural { unNatural :: Integer }  deriving (Eq, Ord)

-- | Natural numbers (including 0): 0, 1, 2, 3, 4, 5, 6, 7, ...
--
-- Internally, this type is represented as an 'Int'.
-- So, it is limited by the 'maxBound' of 'Int'.
--
-- Its 'Enum', 'Listable' and 'Num' instances only produce non-negative values.
-- When @x < y@ then @x - y = 0@.
newtype Nat  =  Nat { unNat :: Int }  deriving (Eq, Ord)

-- | Natural numbers modulo 1: 0
newtype Nat1  =  Nat1 { unNat1 :: Int }  deriving (Eq, Ord)

-- | Natural numbers modulo 2: 0, 1
newtype Nat2  =  Nat2 { unNat2 :: Int }  deriving (Eq, Ord)

-- | Natural numbers modulo 3: 0, 1, 2
newtype Nat3  =  Nat3 { unNat3 :: Int }  deriving (Eq, Ord)

-- | Natural numbers modulo 4: 0, 1, 2, 3
newtype Nat4  =  Nat4 { unNat4 :: Int }  deriving (Eq, Ord)

-- | Natural numbers modulo 5: 0, 1, 2, 3, 4
newtype Nat5  =  Nat5 { unNat5 :: Int }  deriving (Eq, Ord)

-- | Natural numbers modulo 6: 0, 1, 2, 3, 4, 5
newtype Nat6  =  Nat6 { unNat6 :: Int }  deriving (Eq, Ord)

-- | Natural numbers modulo 7: 0, 1, 2, 3, 4, 5, 6
newtype Nat7  =  Nat7 { unNat7 :: Int }  deriving (Eq, Ord)

-- | Generic type 'A'.
--
-- Can be used to test polymorphic functions with a type variable
-- such as 'take' or 'sort':
--
-- > take :: Int -> [a] -> [a]
-- > sort :: Ord a => [a] -> [a]
--
-- by binding them to the following types:
--
-- > take :: Int -> [A] -> [A]
-- > sort :: [A] -> [A]
--
-- This type is homomorphic to 'Nat6', 'B', 'C', 'D', 'E' and 'F'.
--
-- It is instance to several typeclasses so that it can be used
-- to test functions with type contexts.
newtype A  =  A Int  deriving (Eq, Ord)

-- | Generic type 'B'.
--
-- Can be used to test polymorphic functions with two type variables
-- such as 'map' or 'foldr':
--
-- > map :: (a -> b) -> [a] -> [b]
-- > foldr :: (a -> b -> b) -> b -> [a] -> b
--
-- by binding them to the following types:
--
-- > map :: (A -> B) -> [A] -> [B]
-- > foldr :: (A -> B -> B) -> B -> [A] -> B
--
-- This type is homomorphic to 'A', 'Nat6', 'C', 'D', 'E' and 'F'.
newtype B  =  B Int  deriving (Eq, Ord)

-- | Generic type 'C'.
--
-- Can be used to test polymorphic functions with three type variables
-- such as 'uncurry' or 'zipWith':
--
-- > uncurry :: (a -> b -> c) -> (a, b) -> c
-- > zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--
-- by binding them to the following types:
--
-- > uncurry :: (A -> B -> C) -> (A, B) -> C
-- > zipWith :: (A -> B -> C) -> [A] -> [B] -> [C]
--
-- This type is homomorphic to 'A', 'B', 'Nat6', 'D', 'E' and 'F'.
newtype C  =  C Int  deriving (Eq, Ord)

-- | Generic type 'D'.
--
-- Can be used to test polymorphic functions with four type variables.
--
-- This type is homomorphic to 'A', 'B', 'C', 'Nat6', 'E' and 'F'.
newtype D  =  D Int  deriving (Eq, Ord)

-- | Generic type 'E'.
--
-- Can be used to test polymorphic functions with five type variables.
--
-- This type is homomorphic to 'A', 'B', 'C', 'D', 'Nat6' and 'F'.
newtype E  =  E Int  deriving (Eq, Ord)

-- | Generic type 'F'.
--
-- Can be used to test polymorphic functions with five type variables.
--
-- This type is homomorphic to 'A', 'B', 'C', 'D', 'E' and 'Nat6'.
newtype F  =  F Int  deriving (Eq, Ord)

unA :: A -> Int;  unA (A n)  =  n
unB :: B -> Int;  unB (B n)  =  n
unC :: C -> Int;  unC (C n)  =  n
unD :: D -> Int;  unD (D n)  =  n
unE :: E -> Int;  unE (E n)  =  n
unF :: F -> Int;  unF (F n)  =  n

negativeToZero :: (Ord a, Num a) => a -> a
negativeToZero x  | x < 0      =  0
                  | otherwise  =  x

int1  :: Int -> Int1;   int1   =  Int1  . narrowS 1
int2  :: Int -> Int2;   int2   =  Int2  . narrowS 2
int3  :: Int -> Int3;   int3   =  Int3  . narrowS 3
int4  :: Int -> Int4;   int4   =  Int4  . narrowS 4
word1 :: Int -> Word1;  word1  =  Word1 . narrowU 1
word2 :: Int -> Word2;  word2  =  Word2 . narrowU 2
word3 :: Int -> Word3;  word3  =  Word3 . narrowU 3
word4 :: Int -> Word4;  word4  =  Word4 . narrowU 4
nat1 :: Int -> Nat1;  nat1  =  Nat1 . (`mod` 1)
nat2 :: Int -> Nat2;  nat2  =  Nat2 . (`mod` 2)
nat3 :: Int -> Nat3;  nat3  =  Nat3 . (`mod` 3)
nat4 :: Int -> Nat4;  nat4  =  Nat4 . (`mod` 4)
nat5 :: Int -> Nat5;  nat5  =  Nat5 . (`mod` 5)
nat6 :: Int -> Nat6;  nat6  =  Nat6 . (`mod` 6)
nat7 :: Int -> Nat7;  nat7  =  Nat7 . (`mod` 7)
nat  :: Int -> Nat;   nat   =  Nat  . negativeToZero
natural :: Integer -> Natural;  natural  =  Natural . negativeToZero
mkA :: Int -> A;  mkA  =  A . (`mod` 6)
mkB :: Int -> B;  mkB  =  B . (`mod` 6)
mkC :: Int -> C;  mkC  =  C . (`mod` 6)
mkD :: Int -> D;  mkD  =  D . (`mod` 6)
mkE :: Int -> E;  mkE  =  E . (`mod` 6)
mkF :: Int -> F;  mkF  =  F . (`mod` 6)

type ONewtype a  =  (Int -> Int -> Int) -> (a -> a -> a)

oInt1  :: ONewtype Int1;   oInt1   =  oNewtype int1  unInt1
oInt2  :: ONewtype Int2;   oInt2   =  oNewtype int2  unInt2
oInt3  :: ONewtype Int3;   oInt3   =  oNewtype int3  unInt3
oInt4  :: ONewtype Int4;   oInt4   =  oNewtype int4  unInt4
oWord1 :: ONewtype Word1;  oWord1  =  oNewtype word1 unWord1
oWord2 :: ONewtype Word2;  oWord2  =  oNewtype word2 unWord2
oWord3 :: ONewtype Word3;  oWord3  =  oNewtype word3 unWord3
oWord4 :: ONewtype Word4;  oWord4  =  oNewtype word4 unWord4
oNat   :: ONewtype Nat;    oNat    =  oNewtype nat   unNat
oNat1  :: ONewtype Nat1;   oNat1   =  oNewtype nat1  unNat1
oNat2  :: ONewtype Nat2;   oNat2   =  oNewtype nat2  unNat2
oNat3  :: ONewtype Nat3;   oNat3   =  oNewtype nat3  unNat3
oNat4  :: ONewtype Nat4;   oNat4   =  oNewtype nat4  unNat4
oNat5  :: ONewtype Nat5;   oNat5   =  oNewtype nat5  unNat5
oNat6  :: ONewtype Nat6;   oNat6   =  oNewtype nat6  unNat6
oNat7  :: ONewtype Nat7;   oNat7   =  oNewtype nat7  unNat7
oNatural :: (Integer -> Integer -> Integer) -> (Natural -> Natural -> Natural)
oNatural  =  oNewtype natural unNatural
oA :: ONewtype A;  oA  =  oNewtype mkA unA
oB :: ONewtype B;  oB  =  oNewtype mkB unB
oC :: ONewtype C;  oC  =  oNewtype mkC unC
oD :: ONewtype D;  oD  =  oNewtype mkD unD
oE :: ONewtype E;  oE  =  oNewtype mkE unE
oF :: ONewtype F;  oF  =  oNewtype mkF unF

fInt1  :: (Int->Int) -> (Int1->Int1)  ;  fInt1   =  fNewtype int1  unInt1
fInt2  :: (Int->Int) -> (Int2->Int2)  ;  fInt2   =  fNewtype int2  unInt2
fInt3  :: (Int->Int) -> (Int3->Int3)  ;  fInt3   =  fNewtype int3  unInt3
fInt4  :: (Int->Int) -> (Int4->Int4)  ;  fInt4   =  fNewtype int4  unInt4
fWord1 :: (Int->Int) -> (Word1->Word1);  fWord1  =  fNewtype word1 unWord1
fWord2 :: (Int->Int) -> (Word2->Word2);  fWord2  =  fNewtype word2 unWord2
fWord3 :: (Int->Int) -> (Word3->Word3);  fWord3  =  fNewtype word3 unWord3
fWord4 :: (Int->Int) -> (Word4->Word4);  fWord4  =  fNewtype word4 unWord4
fNat   :: (Int->Int) -> (Nat->Nat)    ;  fNat    =  fNewtype Nat   unNat
fNat1  :: (Int->Int) -> (Nat1->Nat1)  ;  fNat1   =  fNewtype nat1  unNat1
fNat2  :: (Int->Int) -> (Nat2->Nat2)  ;  fNat2   =  fNewtype nat2  unNat2
fNat3  :: (Int->Int) -> (Nat3->Nat3)  ;  fNat3   =  fNewtype nat3  unNat3
fNat4  :: (Int->Int) -> (Nat4->Nat4)  ;  fNat4   =  fNewtype nat4  unNat4
fNat5  :: (Int->Int) -> (Nat5->Nat5)  ;  fNat5   =  fNewtype nat5  unNat5
fNat6  :: (Int->Int) -> (Nat6->Nat6)  ;  fNat6   =  fNewtype nat6  unNat6
fNat7  :: (Int->Int) -> (Nat7->Nat7)  ;  fNat7   =  fNewtype nat7  unNat7
fNatural :: (Integer->Integer) -> (Natural->Natural)
fNatural  =  fNewtype Natural unNatural
fA :: (Int -> Int) -> (A -> A);  fA  =  fNewtype mkA unA
fB :: (Int -> Int) -> (B -> B);  fB  =  fNewtype mkB unB
fC :: (Int -> Int) -> (C -> C);  fC  =  fNewtype mkC unC
fD :: (Int -> Int) -> (D -> D);  fD  =  fNewtype mkD unD
fE :: (Int -> Int) -> (E -> E);  fE  =  fNewtype mkE unE
fF :: (Int -> Int) -> (F -> F);  fF  =  fNewtype mkF unF

instance Show Int1 where  show  =  show . unInt1
instance Show Int2 where  show  =  show . unInt2
instance Show Int3 where  show  =  show . unInt3
instance Show Int4 where  show  =  show . unInt4
instance Show Word1 where  show  =  show . unWord1
instance Show Word2 where  show  =  show . unWord2
instance Show Word3 where  show  =  show . unWord3
instance Show Word4 where  show  =  show . unWord4
instance Show Nat where  show (Nat x)  =  show x
instance Show Nat1 where  show  =  show . unNat1
instance Show Nat2 where  show  =  show . unNat2
instance Show Nat3 where  show  =  show . unNat3
instance Show Nat4 where  show  =  show . unNat4
instance Show Nat5 where  show  =  show . unNat5
instance Show Nat6 where  show  =  show . unNat6
instance Show Nat7 where  show  =  show . unNat7
instance Show Natural where  show (Natural x)  =  show x
instance Show A where  show  =  show . unA
instance Show B where  show  =  show . unB
instance Show C where  show  =  show . unC
instance Show D where  show  =  show . unD
instance Show E where  show  =  show . unE
instance Show F where  show  =  show . unF

instance Read Int1 where  readsPrec  =  readsPrecNewtype int1
instance Read Int2 where  readsPrec  =  readsPrecNewtype int2
instance Read Int3 where  readsPrec  =  readsPrecNewtype int3
instance Read Int4 where  readsPrec  =  readsPrecNewtype int4
instance Read Word1 where  readsPrec  =  readsPrecNewtype word1
instance Read Word2 where  readsPrec  =  readsPrecNewtype word2
instance Read Word3 where  readsPrec  =  readsPrecNewtype word3
instance Read Word4 where  readsPrec  =  readsPrecNewtype word4
instance Read Nat where  readsPrec  =  readsPrecNewtype nat
instance Read Nat1 where  readsPrec  =  readsPrecNewtype nat1
instance Read Nat2 where  readsPrec  =  readsPrecNewtype nat2
instance Read Nat3 where  readsPrec  =  readsPrecNewtype nat3
instance Read Nat4 where  readsPrec  =  readsPrecNewtype nat4
instance Read Nat5 where  readsPrec  =  readsPrecNewtype nat5
instance Read Nat6 where  readsPrec  =  readsPrecNewtype nat6
instance Read Nat7 where  readsPrec  =  readsPrecNewtype nat7
instance Read Natural where  readsPrec  =  readsPrecNewtype natural
instance Read A where  readsPrec  =  readsPrecNewtype mkA
instance Read B where  readsPrec  =  readsPrecNewtype mkB
instance Read C where  readsPrec  =  readsPrecNewtype mkC
instance Read D where  readsPrec  =  readsPrecNewtype mkD
instance Read E where  readsPrec  =  readsPrecNewtype mkE
instance Read F where  readsPrec  =  readsPrecNewtype mkF


instance Num Int1 where
  (+)  =  oInt1 (+);  abs     =  fInt1 abs
  (-)  =  oInt1 (-);  signum  =  fInt1 signum
  (*)  =  oInt1 (*);  fromInteger  =  int1 . fromInteger

instance Num Int2 where
  (+)  =  oInt2 (+);  abs     =  fInt2 abs
  (-)  =  oInt2 (-);  signum  =  fInt2 signum
  (*)  =  oInt2 (*);  fromInteger  =  int2 . fromInteger

instance Num Int3 where
  (+)  =  oInt3 (+);  abs     =  fInt3 abs
  (-)  =  oInt3 (-);  signum  =  fInt3 signum
  (*)  =  oInt3 (*);  fromInteger  =  int3 . fromInteger

instance Num Int4 where
  (+)  =  oInt4 (+);  abs     =  fInt4 abs
  (-)  =  oInt4 (-);  signum  =  fInt4 signum
  (*)  =  oInt4 (*);  fromInteger  =  int4 . fromInteger

instance Num Word1 where
  (+)  =  oWord1 (+);  abs     =  fWord1 abs
  (-)  =  oWord1 (-);  signum  =  fWord1 signum
  (*)  =  oWord1 (*);  fromInteger  =  word1 . fromInteger

instance Num Word2 where
  (+)  =  oWord2 (+);  abs     =  fWord2 abs
  (-)  =  oWord2 (-);  signum  =  fWord2 signum
  (*)  =  oWord2 (*);  fromInteger  =  word2 . fromInteger

instance Num Word3 where
  (+)  =  oWord3 (+);  abs     =  fWord3 abs
  (-)  =  oWord3 (-);  signum  =  fWord3 signum
  (*)  =  oWord3 (*);  fromInteger  =  word3 . fromInteger

instance Num Word4 where
  (+)  =  oWord4 (+);  abs     =  fWord4 abs
  (-)  =  oWord4 (-);  signum  =  fWord4 signum
  (*)  =  oWord4 (*);  fromInteger  =  word4 . fromInteger

instance Num Nat where
  (+)  =  oNat (+);  abs     =  fNat abs
  (-)  =  oNat (-);  signum  =  fNat signum
  (*)  =  oNat (*);  fromInteger  =  nat . fromInteger

instance Num Nat1 where
  (+)  =  oNat1 (+);  abs     =  fNat1 abs
  (-)  =  oNat1 (-);  signum  =  fNat1 signum
  (*)  =  oNat1 (*);  fromInteger  =  nat1 . fromInteger

instance Num Nat2 where
  (+)  =  oNat2 (+);  abs     =  fNat2 abs
  (-)  =  oNat2 (-);  signum  =  fNat2 signum
  (*)  =  oNat2 (*);  fromInteger  =  nat2 . fromInteger

instance Num Nat3 where
  (+)  =  oNat3 (+);  abs     =  fNat3 abs
  (-)  =  oNat3 (-);  signum  =  fNat3 signum
  (*)  =  oNat3 (*);  fromInteger  =  nat3 . fromInteger

instance Num Nat4 where
  (+)  =  oNat4 (+);  abs     =  fNat4 abs
  (-)  =  oNat4 (-);  signum  =  fNat4 signum
  (*)  =  oNat4 (*);  fromInteger  =  nat4 . fromInteger

instance Num Nat5 where
  (+)  =  oNat5 (+);  abs     =  fNat5 abs
  (-)  =  oNat5 (-);  signum  =  fNat5 signum
  (*)  =  oNat5 (*);  fromInteger  =  nat5 . fromInteger

instance Num Nat6 where
  (+)  =  oNat6 (+);  abs     =  fNat6 abs
  (-)  =  oNat6 (-);  signum  =  fNat6 signum
  (*)  =  oNat6 (*);  fromInteger  =  nat6 . fromInteger

instance Num Nat7 where
  (+)  =  oNat7 (+);  abs     =  fNat7 abs
  (-)  =  oNat7 (-);  signum  =  fNat7 signum
  (*)  =  oNat7 (*);  fromInteger  =  nat7 . fromInteger

instance Num Natural where
  (+)  =  oNatural (+);  abs     =  fNatural abs
  (-)  =  oNatural (-);  signum  =  fNatural signum
  (*)  =  oNatural (*);  fromInteger  =  natural . fromInteger

instance Num A where
  (+)  =  oA (+);  abs     =  fA abs
  (-)  =  oA (-);  signum  =  fA signum
  (*)  =  oA (*);  fromInteger  =  mkA . fromInteger

instance Num B where
  (+)  =  oB (+);  abs     =  fB abs
  (-)  =  oB (-);  signum  =  fB signum
  (*)  =  oB (*);  fromInteger  =  mkB . fromInteger

instance Num C where
  (+)  =  oC (+);  abs     =  fC abs
  (-)  =  oC (-);  signum  =  fC signum
  (*)  =  oC (*);  fromInteger  =  mkC . fromInteger

instance Num D where
  (+)  =  oD (+);  abs     =  fD abs
  (-)  =  oD (-);  signum  =  fD signum
  (*)  =  oD (*);  fromInteger  =  mkD . fromInteger

instance Num E where
  (+)  =  oE (+);  abs     =  fE abs
  (-)  =  oE (-);  signum  =  fE signum
  (*)  =  oE (*);  fromInteger  =  mkE . fromInteger

instance Num F where
  (+)  =  oF (+);  abs     =  fF abs
  (-)  =  oF (-);  signum  =  fF signum
  (*)  =  oF (*);  fromInteger  =  mkF . fromInteger


instance Real Int1 where  toRational (Int1 x)  =  fromIntegral x % 1
instance Real Int2 where  toRational (Int2 x)  =  fromIntegral x % 1
instance Real Int3 where  toRational (Int3 x)  =  fromIntegral x % 1
instance Real Int4 where  toRational (Int4 x)  =  fromIntegral x % 1
instance Real Word1 where  toRational (Word1 x)  =  fromIntegral x % 1
instance Real Word2 where  toRational (Word2 x)  =  fromIntegral x % 1
instance Real Word3 where  toRational (Word3 x)  =  fromIntegral x % 1
instance Real Word4 where  toRational (Word4 x)  =  fromIntegral x % 1
instance Real Nat where  toRational (Nat x)  =  fromIntegral x % 1
instance Real Nat1 where  toRational (Nat1 x)  =  fromIntegral x % 1
instance Real Nat2 where  toRational (Nat2 x)  =  fromIntegral x % 1
instance Real Nat3 where  toRational (Nat3 x)  =  fromIntegral x % 1
instance Real Nat4 where  toRational (Nat4 x)  =  fromIntegral x % 1
instance Real Nat5 where  toRational (Nat5 x)  =  fromIntegral x % 1
instance Real Nat6 where  toRational (Nat6 x)  =  fromIntegral x % 1
instance Real Nat7 where  toRational (Nat7 x)  =  fromIntegral x % 1
instance Real Natural where  toRational (Natural x)  =  fromIntegral x % 1
instance Real A where  toRational (A x)  =  fromIntegral x % 1
instance Real B where  toRational (B x)  =  fromIntegral x % 1
instance Real C where  toRational (C x)  =  fromIntegral x % 1
instance Real D where  toRational (D x)  =  fromIntegral x % 1
instance Real E where  toRational (E x)  =  fromIntegral x % 1
instance Real F where  toRational (F x)  =  fromIntegral x % 1

instance Integral Int1 where  quotRem  =  otNewtype int1 unInt1 quotRem
                              toInteger  =  toInteger . unInt1

instance Integral Int2 where  quotRem  =  otNewtype int2 unInt2 quotRem
                              toInteger  =  toInteger . unInt2

instance Integral Int3 where  quotRem  =  otNewtype int3 unInt3 quotRem
                              toInteger  =  toInteger . unInt3

instance Integral Int4 where  quotRem  =  otNewtype int4 unInt4 quotRem
                              toInteger  =  toInteger . unInt4

instance Integral Word1 where  quotRem  =  otNewtype word1 unWord1 quotRem
                               toInteger  =  toInteger . unWord1

instance Integral Word2 where  quotRem  =  otNewtype word2 unWord2 quotRem
                               toInteger  =  toInteger . unWord2

instance Integral Word3 where  quotRem  =  otNewtype word3 unWord3 quotRem
                               toInteger  =  toInteger . unWord3

instance Integral Word4 where  quotRem  =  otNewtype word4 unWord4 quotRem
                               toInteger  =  toInteger . unWord4

instance Integral Nat where  quotRem  =  otNewtype Nat unNat quotRem
                             toInteger  =  toInteger . unNat

instance Integral Nat1 where  quotRem  =  otNewtype nat1 unNat1 quotRem
                              toInteger  =  toInteger . unNat1

instance Integral Nat2 where  quotRem  =  otNewtype nat2 unNat2 quotRem
                              toInteger  =  toInteger . unNat2

instance Integral Nat3 where  quotRem  =  otNewtype nat3 unNat3 quotRem
                              toInteger  =  toInteger . unNat3

instance Integral Nat4 where  quotRem  =  otNewtype nat4 unNat4 quotRem
                              toInteger  =  toInteger . unNat4

instance Integral Nat5 where  quotRem  =  otNewtype nat5 unNat5 quotRem
                              toInteger  =  toInteger . unNat5

instance Integral Nat6 where  quotRem  =  otNewtype nat6 unNat6 quotRem
                              toInteger  =  toInteger . unNat6

instance Integral Nat7 where  quotRem  =  otNewtype nat7 unNat7 quotRem
                              toInteger  =  toInteger . unNat7

instance Integral Natural where  quotRem  =  otNewtype natural unNatural quotRem
                                 toInteger  =  toInteger . unNatural

instance Integral A where  quotRem  =  otNewtype mkA unA quotRem
                           toInteger  =  toInteger . unA

instance Integral B where  quotRem  =  otNewtype mkB unB quotRem
                           toInteger  =  toInteger . unB

instance Integral C where  quotRem  =  otNewtype mkC unC quotRem
                           toInteger  =  toInteger . unC

instance Integral D where  quotRem  =  otNewtype mkD unD quotRem
                           toInteger  =  toInteger . unD

instance Integral E where  quotRem  =  otNewtype mkE unE quotRem
                           toInteger  =  toInteger . unE

instance Integral F where  quotRem  =  otNewtype mkF unF quotRem
                           toInteger  =  toInteger . unF

instance Bounded Int1 where  maxBound  =  Int1 0;  minBound  =  Int1 (-1)
instance Bounded Int2 where  maxBound  =  Int2 1;  minBound  =  Int2 (-2)
instance Bounded Int3 where  maxBound  =  Int3 3;  minBound  =  Int3 (-4)
instance Bounded Int4 where  maxBound  =  Int4 7;  minBound  =  Int4 (-8)
instance Bounded Word1 where  maxBound  =  Word1 1;  minBound  =  Word1 0
instance Bounded Word2 where  maxBound  =  Word2 3;  minBound  =  Word2 0
instance Bounded Word3 where  maxBound  =  Word3 7;  minBound  =  Word3 0
instance Bounded Word4 where  maxBound  =  Word4 15;  minBound  =  Word4 0
instance Bounded Nat where  maxBound  =  Nat maxBound;  minBound  =  Nat 0
instance Bounded Nat1 where  maxBound  =  Nat1 0;  minBound  =  Nat1 0
instance Bounded Nat2 where  maxBound  =  Nat2 1;  minBound  =  Nat2 0
instance Bounded Nat3 where  maxBound  =  Nat3 2;  minBound  =  Nat3 0
instance Bounded Nat4 where  maxBound  =  Nat4 3;  minBound  =  Nat4 0
instance Bounded Nat5 where  maxBound  =  Nat5 4;  minBound  =  Nat5 0
instance Bounded Nat6 where  maxBound  =  Nat6 5;  minBound  =  Nat6 0
instance Bounded Nat7 where  maxBound  =  Nat7 6;  minBound  =  Nat7 0
instance Bounded A where  maxBound  =  A 5;  minBound  =  A 0
instance Bounded B where  maxBound  =  B 5;  minBound  =  B 0
instance Bounded C where  maxBound  =  C 5;  minBound  =  C 0
instance Bounded D where  maxBound  =  D 5;  minBound  =  D 0
instance Bounded E where  maxBound  =  E 5;  minBound  =  E 0
instance Bounded F where  maxBound  =  F 5;  minBound  =  F 0

instance Enum Int1 where
  toEnum    =  int1;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unInt1;  enumFromThen  =  boundedEnumFromThen

instance Enum Int2 where
  toEnum    =  int2;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unInt2;  enumFromThen  =  boundedEnumFromThen

instance Enum Int3 where
  toEnum    =  int3;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unInt3;  enumFromThen  =  boundedEnumFromThen

instance Enum Int4 where
  toEnum    =  int4;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unInt4;  enumFromThen  =  boundedEnumFromThen

instance Enum Word1 where
  toEnum    =  word1;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unWord1;  enumFromThen  =  boundedEnumFromThen

instance Enum Word2 where
  toEnum    =  word2;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unWord2;  enumFromThen  =  boundedEnumFromThen

instance Enum Word3 where
  toEnum    =  word3;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unWord3;  enumFromThen  =  boundedEnumFromThen

instance Enum Word4 where
  toEnum    =  word4;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unWord4;  enumFromThen  =  boundedEnumFromThen

instance Enum Nat where
  toEnum    =  nat;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unNat;  enumFromThen  =  boundedEnumFromThen

instance Enum Nat1 where
  toEnum    =  nat1;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unNat1;  enumFromThen  =  boundedEnumFromThen

instance Enum Nat2 where
  toEnum    =  nat2;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unNat2;  enumFromThen  =  boundedEnumFromThen

instance Enum Nat3 where
  toEnum    =  nat3;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unNat3;  enumFromThen  =  boundedEnumFromThen

instance Enum Nat4 where
  toEnum    =  nat4;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unNat4;  enumFromThen  =  boundedEnumFromThen

instance Enum Nat5 where
  toEnum    =  nat5;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unNat5;  enumFromThen  =  boundedEnumFromThen

instance Enum Nat6 where
  toEnum    =  nat6;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unNat6;  enumFromThen  =  boundedEnumFromThen

instance Enum Nat7 where
  toEnum    =  nat7;    enumFrom      =  boundedEnumFrom
  fromEnum  =  unNat7;  enumFromThen  =  boundedEnumFromThen

instance Enum Natural where
  toEnum    =  natural . fromIntegral
  fromEnum  =  fromInteger . unNatural
  enumFrom     (Natural x)              =  map Natural [x..]
  enumFromThen (Natural x) (Natural s)  =  map Natural [x,s..]

instance Enum A where
  toEnum    =  mkA;  enumFrom      =  boundedEnumFrom
  fromEnum  =  unA;  enumFromThen  =  boundedEnumFromThen

instance Enum B where
  toEnum    =  mkB;  enumFrom      =  boundedEnumFrom
  fromEnum  =  unB;  enumFromThen  =  boundedEnumFromThen

instance Enum C where
  toEnum    =  mkC;  enumFrom      =  boundedEnumFrom
  fromEnum  =  unC;  enumFromThen  =  boundedEnumFromThen

instance Enum D where
  toEnum    =  mkD;  enumFrom      =  boundedEnumFrom
  fromEnum  =  unD;  enumFromThen  =  boundedEnumFromThen

instance Enum E where
  toEnum    =  mkE;  enumFrom      =  boundedEnumFrom
  fromEnum  =  unE;  enumFromThen  =  boundedEnumFromThen

instance Enum F where toEnum    =  mkF;  enumFrom      =  boundedEnumFrom
                      fromEnum  =  unF;  enumFromThen  =  boundedEnumFromThen

rng :: Enum a => (a,a) -> [a]
rng (m,n)  =  [m..n]

idx :: Integral a => (a,a) -> a -> Int
idx b@(m,_) i  | irng b i   =  fromIntegral (i - m)
               | otherwise  =  error "Test.LeanCheck.Utils.Types.idx: index out of range"

irng :: Ord a => (a,a) -> a -> Bool
irng (m,n) i  =  m <= i && i <= m

instance Ix Int1    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Int2    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Int3    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Int4    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Word1   where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Word2   where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Word3   where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Word4   where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Nat     where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Nat1    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Nat2    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Nat3    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Nat4    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Nat5    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Nat6    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Nat7    where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix Natural where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix A       where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix B       where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix C       where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix D       where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix E       where  range  =  rng;  index  =  idx;  inRange  =  irng
instance Ix F       where  range  =  rng;  index  =  idx;  inRange  =  irng

instance Listable Int1 where  list  =  [0,minBound]
instance Listable Int2 where  list  =  listIntegral
instance Listable Int3 where  list  =  listIntegral
instance Listable Int4 where  list  =  listIntegral
instance Listable Word1 where  list  =  listIntegral
instance Listable Word2 where  list  =  listIntegral
instance Listable Word3 where  list  =  listIntegral
instance Listable Word4 where  list  =  listIntegral
instance Listable Nat where  list  =  listIntegral
instance Listable Nat1 where  list  =  listIntegral
instance Listable Nat2 where  list  =  listIntegral
instance Listable Nat3 where  list  =  listIntegral
instance Listable Nat4 where  list  =  listIntegral
instance Listable Nat5 where  list  =  listIntegral
instance Listable Nat6 where  list  =  listIntegral
instance Listable Nat7 where  list  =  listIntegral
instance Listable Natural where  list  =  listIntegral
instance Listable A where  list  =  listIntegral
instance Listable B where  list  =  listIntegral
instance Listable C where  list  =  listIntegral
instance Listable D where  list  =  listIntegral
instance Listable E where  list  =  listIntegral
instance Listable F where  list  =  listIntegral

-- | Deprecated.  Use 'Word1'.
type UInt1  =  Word1

-- | Deprecated.  Use 'Word2'.
type UInt2  =  Word2

-- | Deprecated.  Use 'Word3'.
type UInt3  =  Word3

-- | Deprecated.  Use 'Word4'.
type UInt4  =  Word4

-- | Lists without repeated elements.
--
-- > > take 6 $ list :: [NoDup Nat]
-- > [NoDup [],NoDup [0],NoDup [1],NoDup [0,1],NoDup [1,0],NoDup [2]]
--
-- Example, checking the property that @nub@ is an identity:
--
-- > import Data.List (nub)
-- > > check $ \xs -> nub xs == (xs :: [Int])
-- > *** Failed! Falsifiable (after 3 tests):
-- > [0,0]
-- > > check $ \(NoDup xs) -> nub xs == (xs :: [Int])
-- > +++ OK, passed 200 tests.
newtype NoDup a  =  NoDup [a]  deriving (Show, Read, Eq, Ord)

-- | Lists representing bags (multisets).
--   The 'Listable' 'tiers' enumeration will not have repeated bags.
--
-- > > take 6 (list :: [Bag Nat])
-- > [Bag [],Bag [0],Bag [0,0],Bag [1],Bag [0,0,0],Bag [0,1]]
--
-- See also: 'Test.LeanCheck.Tiers.bagsOf' and 'bagCons'.
newtype Bag a  =  Bag [a]  deriving (Show, Read, Eq, Ord)

-- | Lists representing sets.
--   The 'Listable' 'tiers' enumeration will not have repeated sets.
--
-- > > take 6 (list :: [Set Nat])
-- > [Set [],Set [0],Set [1],Set [0,1],Set [2],Set [0,2]]
newtype Set a  =  Set [a]  deriving (Show, Read, Eq, Ord)

-- | Lists of pairs representing maps.
--   The 'Listable' 'tiers' enumeration will not have repeated maps.
--
-- > > take 6 (list :: [Map Nat Nat])
-- > [Map [],Map [(0,0)],Map [(0,1)],Map [(1,0)],Map [(0,2)],Map [(1,1)]]
newtype Map a b  =  Map [(a,b)]  deriving (Show, Read, Eq, Ord)

instance Listable a => Listable (NoDup a) where  tiers  =  noDupListCons NoDup
instance Listable a => Listable (Bag a)   where  tiers  =  bagCons Bag
instance Listable a => Listable (Set a)   where  tiers  =  setCons Set
instance (Listable a, Listable b)
                    => Listable (Map a b) where  tiers  =  mapCons Map

-- | 'X' type to be wrapped around integer types for an e-'X'-treme integer
--   enumeration.  See the 'Listable' instance for 'X'.  Use 'X' when
--   testing properties about overflows and the like:
--
-- > > check $ \x -> x + 1 > (x :: Int)
-- > +++ OK, passed 200 tests.
--
-- > > check $ \(X x) -> x + 1 > (x :: Int)
-- > +++ Failed! Falsifiable (after 4 tests):
-- > 9223372036854775807
newtype X a  =  X {unX :: a}  deriving (Eq, Ord)
instance Show a => Show (X a) where  show (X x)  =  show x
instance (Integral a, Bounded a) => Listable (X a) where
  list  =  map X listXIntegral
-- ^ Extremily large integers are intercalated with small integers.
--
--   > list :: [X Int]  =  map X
--   >   [ 0, 1, -1, maxBound,   minBound
--   >      , 2, -2, maxBound-1, minBound+1
--   >      , 3, -3, maxBound-2, minBound+2
--   >      , ... ]

-- FIXME: make this work for Int2 / Word2 types
--        by checking then using normal enumeration
listXIntegral :: (Bounded a, Integral a) => [a]
listXIntegral  =  l undefined
  where
  l :: (Bounded a, Integral a) => a -> [a]
  l a | count a <= 4  =  listIntegral
      | min < 0       =  listXIntegralN
      | otherwise     =  listXIntegralP
    where
    min  =  minBound `asTypeOf` a
-- The type-hackery above is needed so that we don't need to activate
-- ScopedTypeVariables

listXIntegralN :: (Bounded a, Integral a) => [a]
listXIntegralN  =  0 : (extremes 1 maxBound) +| (extremes (-1) minBound)
-- listXIntegralN :: Int4 =
--    0 : (([1,2,3,4] +| [7,6,5]) +| ([-1,-2,-3,-4] +| [-8,-7,-6,-5]))

listXIntegralP :: (Bounded a, Integral a) => [a]
listXIntegralP  =  0 : [1..midBound] ++| [maxBound,(maxBound-1)..(midBound+1)]
  where
  midBound  =  maxBound `div` 3 * 2

-- how many of this type exist?
-- assumes 0 `elem` [minBound..maxBound]
count :: (Bounded a, Integral a) => a -> Integer
count a  =  1 + max + abs min
  where
  min  =  fromIntegral $ minBound `asTypeOf` a
  max  =  fromIntegral $ maxBound `asTypeOf` a

extremes :: (Integral a) => a -> a -> [a]
extremes x y
  | x > y      =  [x,x-1..m] +| [y..m-1]
  | otherwise  =  [x..m] +| [y,y-1..m+1]
  where  m  =  mid x y

mid :: Integral a => a -> a -> a
mid x y  =  x `div` 2
         +  y `div` 2
         +  if odd x && odd y then 1 else 0

-- | Lazily interleaves two lists, switching between elements of the two.
--   This version uses the first list more frequently than the second.
--
-- > [x,y,z,w] +| [a,b] == [x,y, a, z,w, b]
(++|) :: [a] -> [a] -> [a]
[]        ++| ys      =  ys
xs        ++| []      =  xs
[x]       ++| ys      =  x:ys
(x:x':xs) ++| (y:ys)  =  x:x':y:(xs ++| ys)
infixr 5 ++|

-- | Wrap around lists of integers for an enumeration containing e-'X'-treme
--   integer values.
--
-- > > check $ \xs -> all (>=0) xs ==> sum (take 1 xs :: [Int]) <= sum xs
-- > +++ OK, passed 200 tests.
--
-- > > check $ \(Xs xs) -> all (>=0) xs ==> sum (take 1 xs :: [Int]) <= sum xs
-- > *** Failed! Falsifiable (after 56 tests):
-- > [1,9223372036854775807]
newtype Xs a  =  Xs [a]  deriving (Eq, Ord, Show, Read)
instance (Integral a, Bounded a) => Listable (Xs a) where
  tiers  =  cons1 (Xs . map unX)
-- ^ Lists with elements of the 'X' type.

-- | Space characters.
--
-- > list :: [Space]  =  " \t\n\r\f\v"
--
-- > > check $ \(Space c) -> isSpace c
-- > +++ OK, passed 6 tests (exhausted).
newtype Space  =  Space {unSpace :: Char}

-- | Lowercase characters.
--
-- > list :: [Lower]  =  "abcdef..."
--
-- > > check $ \(Lower c) -> isLower c
-- > +++ OK, passed 26 tests (exhausted).
newtype Lower  =  Lower {unLower :: Char}

-- | Uppercase characters.
--
-- > list :: [Upper]  =  "ABCDEF..."
--
-- > > check $ \(Upper c) -> isUpper c
-- > +++ OK, passed 26 tests (exhausted).
newtype Upper  =  Upper {unUpper :: Char}

-- | Alphabetic characters.
--
-- > list :: [Alpha]  =  "aAbBcC..."
--
-- > > check $ \(Alpha c) -> isAlpha c
-- > +++ OK, passed 52 tests (exhausted).
--
-- Equivalent to 'Letter'.
newtype Alpha  =  Alpha {unAlpha :: Char}

-- | Digits.
--
-- > list :: [Digit]  =  "0123456789"
--
-- > > check $ \(Digit c) -> isDigit c
-- > +++ OK, passed 10 tests (exhausted).
newtype Digit  =  Digit {unDigit :: Char}

-- | Alphanumeric characters.
--
-- > list :: [AlphaNum]  =  "0a1A2b3B4c..."
--
-- > > check $ \(AlphaNum c) -> isAlphaNum c
-- > +++ OK, passed 62 tests (exhausted).
newtype AlphaNum  =  AlphaNum {unAlphaNum :: Char}

-- | Alphabetic characters.
--
-- > list :: [Letter]  =  "aAbBcC..."
--
-- > > check $ \(Letter c) -> isLetter c
-- > +++ OK, passed 52 tests (exhausted).
--
-- Equivalent to 'Alpha'.
newtype Letter    =  Letter   {unLetter   :: Char}

instance Show Space where  show  =  show . unSpace
instance Show Lower where  show  =  show . unLower
instance Show Upper where  show  =  show . unUpper
instance Show Alpha where  show  =  show . unAlpha
instance Show Digit where  show  =  show . unDigit
instance Show AlphaNum where  show  =  show . unAlphaNum
instance Show Letter   where  show  =  show . unLetter

instance Listable Space where
  list  =  map Space [' ', '\t', '\n', '\r', '\f', '\v']

instance Listable Lower where
  list  =  map Lower ['a'..'z']

instance Listable Upper where
  list  =  map Upper ['A'..'Z']

instance Listable Alpha where
  list  =  map Alpha $ ['a'..'z'] +| ['A'..'Z']

instance Listable Digit where
  list  =  map Digit ['0'..'9']

instance Listable AlphaNum where
  list  =  map AlphaNum $ ['0'..'9'] +| ['a'..'z'] +| ['A'..'Z']

instance Listable Letter where
  list  =  map Letter $ ['a'..'z'] +| ['A'..'Z']

-- | Strings of spaces.
newtype Spaces  =  Spaces {unSpaces :: String}

-- | Strings of lowercase characters.
newtype Lowers  =  Lowers {unLowers :: String}

-- | Strings of uppercase characters
newtype Uppers  =  Uppers {unUppers :: String}

-- | Strings of alphabetic characters
newtype Alphas  =  Alphas {unAlphas :: String}

-- | Strings of digits.
newtype Digits  =  Digits {unDigits :: String}

-- | Strings of alphanumeric characters
newtype AlphaNums  =  AlphaNums {unAlphaNums :: String}

-- | Strings of letters
newtype Letters    =  Letters   {unLetters   :: String}

instance Show Spaces where  show  =  show . unSpaces
instance Show Lowers where  show  =  show . unLowers
instance Show Uppers where  show  =  show . unUppers
instance Show Alphas where  show  =  show . unAlphas
instance Show Digits where  show  =  show . unDigits
instance Show AlphaNums where  show  =  show . unAlphaNums
instance Show Letters   where  show  =  show . unLetters

instance Listable Spaces where  tiers  =  cons1 (Spaces . map unSpace)
instance Listable Lowers where  tiers  =  cons1 (Lowers . map unLower)
instance Listable Uppers where  tiers  =  cons1 (Uppers . map unUpper)
instance Listable Alphas where  tiers  =  cons1 (Alphas . map unAlpha)
instance Listable Digits where  tiers  =  cons1 (Digits . map unDigit)
instance Listable AlphaNums where  tiers  =  cons1 (AlphaNums . map unAlphaNum)
instance Listable Letters   where  tiers  =  cons1 (Letters   . map unLetter)

#if __GLASGOW_HASKELL__ == 708
-- there's no need to derive these on GHC >= 7.10
-- as they are automatically derived
deriving instance Typeable Int1
deriving instance Typeable Int2
deriving instance Typeable Int3
deriving instance Typeable Int4
deriving instance Typeable Word1
deriving instance Typeable Word2
deriving instance Typeable Word3
deriving instance Typeable Word4
deriving instance Typeable Nat
deriving instance Typeable Nat1
deriving instance Typeable Nat2
deriving instance Typeable Nat3
deriving instance Typeable Nat4
deriving instance Typeable Nat5
deriving instance Typeable Nat6
deriving instance Typeable Nat7
deriving instance Typeable Natural
deriving instance Typeable X
deriving instance Typeable Xs
deriving instance Typeable NoDup
deriving instance Typeable Bag
deriving instance Typeable Set
deriving instance Typeable Map
deriving instance Typeable Space
deriving instance Typeable Lower
deriving instance Typeable Upper
deriving instance Typeable Alpha
deriving instance Typeable Digit
deriving instance Typeable AlphaNum
deriving instance Typeable Letter
deriving instance Typeable Spaces
deriving instance Typeable Lowers
deriving instance Typeable Uppers
deriving instance Typeable Alphas
deriving instance Typeable Digits
deriving instance Typeable AlphaNums
deriving instance Typeable Letters
deriving instance Typeable A
deriving instance Typeable B
deriving instance Typeable C
deriving instance Typeable D
deriving instance Typeable E
deriving instance Typeable F
#endif
