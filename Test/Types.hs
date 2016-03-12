-- | Types to aid in property-based testing.
--
-- NOTE: On these types, due to the way Haskell desugars negation, you cannot
-- write the lower bounds for types with negative values (Int1,Int2,Int3,Int4)
-- directly: see NegativeLiterals extension for a through explanation of this
-- issue.
module Test.Types
  (
  -- * Integer types
    Int1
  , Int2
  , Int3
  , Int4
  , Word1
  , Word2
  , Word3
  , Word4
  , Nat

  -- * Aliases to word types (deprecated)
  , UInt1
  , UInt2
  , UInt3
  , UInt4
  )
where

import Test.Check (Listable(..), listIntegral)
import Data.Ratio ((%))

narrowU :: Int -> Int -> Int
narrowU w i = i `mod` 2^w

narrowS :: Int -> Int -> Int
narrowS w i = let l  = 2^w
                  i' = i `mod` l
              in if i' < 2^(w-1)
                   then i'
                   else i' - l

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple f (x,y) = (f x, f y)

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (x,y) = (f x,y)

oNewtype :: (a -> b) -> (b -> a) -> (a -> a -> a) -> (b -> b -> b)
oNewtype con des o = \x y -> con $ des x `o` des y

fNewtype :: (a -> b) -> (b -> a) -> (a -> a) -> (b -> b)
fNewtype con des f = con . f . des

otNewtype :: (a -> b) -> (b -> a) -> (a -> a -> (a,a)) -> (b -> b -> (b,b))
otNewtype con des o = \x y -> mapTuple con $ des x `o` des y

readsPrecNewtype :: Read a => (a -> b) -> Int -> String -> [(b,String)]
readsPrecNewtype con n = map (mapFst con) . readsPrec n

boundedEnumFrom :: (Ord a,Bounded a,Enum a) => a -> [a]
boundedEnumFrom x = [x..maxBound]

boundedEnumFromThen :: (Ord a,Bounded a,Enum a) => a -> a -> [a]
boundedEnumFromThen x y | x > y     = [x,y..minBound]
                        | otherwise = [x,y..maxBound]


-- Single-bit signed integer: -1, 0
newtype Int1 = Int1 { unInt1 :: Int } deriving (Eq, Ord)

int1 :: Int -> Int1
int1 = Int1 . narrowS 1

instance Show Int1 where
  show = show . unInt1

instance Read Int1 where
  readsPrec = readsPrecNewtype int1

instance Num Int1 where
  (+) = oNewtype int1 unInt1 (+)
  (-) = oNewtype int1 unInt1 (-)
  (*) = oNewtype int1 unInt1 (*)
  abs = fNewtype int1 unInt1 abs
  signum = fNewtype int1 unInt1 signum
  fromInteger = int1 . fromInteger

instance Real Int1 where
  toRational (Int1 x) = fromIntegral x % 1

instance Integral Int1 where
  quotRem = otNewtype int1 unInt1 quotRem
  toInteger = toInteger . unInt1

instance Bounded Int1 where
  maxBound = Int1 0
  minBound = Int1 (-1)

instance Enum Int1 where
  toEnum   = int1
  fromEnum = unInt1
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Int1 where
  list = [0,minBound]


-- Single-bit unsigned integer: 0, 1
newtype Word1 = Word1 { unWord1 :: Int } deriving (Eq, Ord)

word1 :: Int -> Word1
word1 = Word1 . narrowU 1

instance Show Word1 where
  show = show . unWord1

instance Read Word1 where
  readsPrec = readsPrecNewtype word1

instance Num Word1 where
  (+) = oNewtype word1 unWord1 (+)
  (-) = oNewtype word1 unWord1 (-)
  (*) = oNewtype word1 unWord1 (*)
  abs = fNewtype word1 unWord1 abs
  signum = fNewtype word1 unWord1 signum
  fromInteger = word1 . fromInteger

instance Real Word1 where
  toRational (Word1 x) = fromIntegral x % 1

instance Integral Word1 where
  quotRem = otNewtype word1 unWord1 quotRem
  toInteger = toInteger . unWord1

instance Bounded Word1 where
  maxBound = Word1 1
  minBound = Word1 0

instance Enum Word1 where
  toEnum   = word1
  fromEnum = unWord1
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Word1 where
  list = [0,maxBound]


-- Two-bit signed integer: -2, -1, 0, 1
newtype Int2 = Int2 { unInt2 :: Int } deriving (Eq, Ord)

int2 :: Int -> Int2
int2 = Int2 . narrowS 2

instance Show Int2 where
  show = show . unInt2

instance Read Int2 where
  readsPrec = readsPrecNewtype int2

instance Num Int2 where
  (+) = oNewtype int2 unInt2 (+)
  (-) = oNewtype int2 unInt2 (-)
  (*) = oNewtype int2 unInt2 (*)
  abs = fNewtype int2 unInt2 abs
  signum = fNewtype int2 unInt2 signum
  fromInteger = int2 . fromInteger

instance Real Int2 where
  toRational (Int2 x) = fromIntegral x % 1

instance Integral Int2 where
  quotRem = otNewtype int2 unInt2 quotRem
  toInteger = toInteger . unInt2

instance Bounded Int2 where
  maxBound = Int2 1
  minBound = Int2 (-2)

instance Enum Int2 where
  toEnum   = int2
  fromEnum = unInt2
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Int2 where
  list = listIntegral


-- Two-bit unsigned integer: 0, 1, 2, 3
newtype Word2 = Word2 { unWord2 :: Int } deriving (Eq, Ord)

word2 :: Int -> Word2
word2 = Word2 . narrowU 2

instance Show Word2 where
  show = show . unWord2

instance Read Word2 where
  readsPrec = readsPrecNewtype word2

instance Num Word2 where
  (+) = oNewtype word2 unWord2 (+)
  (-) = oNewtype word2 unWord2 (-)
  (*) = oNewtype word2 unWord2 (*)
  abs = fNewtype word2 unWord2 abs
  signum = fNewtype word2 unWord2 signum
  fromInteger = word2 . fromInteger

instance Real Word2 where
  toRational (Word2 x) = fromIntegral x % 1

instance Integral Word2 where
  quotRem = otNewtype word2 unWord2 quotRem
  toInteger = toInteger . unWord2

instance Bounded Word2 where
  maxBound = Word2 3
  minBound = Word2 0

instance Enum Word2 where
  toEnum   = word2
  fromEnum = unWord2
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Word2 where
  list = [0..]


-- Three-bit signed integer: -4, -3, -2, -1, 0, 1, 2, 3
newtype Int3 = Int3 { unInt3 :: Int } deriving (Eq, Ord)

int3 :: Int -> Int3
int3 = Int3 . narrowS 3

instance Show Int3 where
  show = show . unInt3

instance Read Int3 where
  readsPrec = readsPrecNewtype int3

instance Num Int3 where
  (+) = oNewtype int3 unInt3 (+)
  (-) = oNewtype int3 unInt3 (-)
  (*) = oNewtype int3 unInt3 (*)
  abs = fNewtype int3 unInt3 abs
  signum = fNewtype int3 unInt3 signum
  fromInteger = int3 . fromInteger

instance Real Int3 where
  toRational (Int3 x) = fromIntegral x % 1

instance Integral Int3 where
  quotRem = otNewtype int3 unInt3 quotRem
  toInteger = toInteger . unInt3

instance Bounded Int3 where
  maxBound = Int3 3
  minBound = Int3 (-4)

instance Enum Int3 where
  toEnum   = int3
  fromEnum = unInt3
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Int3 where
  list = listIntegral


-- Three-bit unsigned integer: 0, 1, 2, 3, 4, 5, 6, 7
newtype Word3 = Word3 { unWord3 :: Int } deriving (Eq, Ord)

word3 :: Int -> Word3
word3 = Word3 . narrowU 3

instance Show Word3 where
  show = show . unWord3

instance Read Word3 where
  readsPrec = readsPrecNewtype word3

instance Num Word3 where
  (+) = oNewtype word3 unWord3 (+)
  (-) = oNewtype word3 unWord3 (-)
  (*) = oNewtype word3 unWord3 (*)
  abs = fNewtype word3 unWord3 abs
  signum = fNewtype word3 unWord3 signum
  fromInteger = word3 . fromInteger

instance Real Word3 where
  toRational (Word3 x) = fromIntegral x % 1

instance Integral Word3 where
  quotRem = otNewtype word3 unWord3 quotRem
  toInteger = toInteger . unWord3

instance Bounded Word3 where
  maxBound = Word3 7
  minBound = Word3 0

instance Enum Word3 where
  toEnum   = word3
  fromEnum = unWord3
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Word3 where
  list = [0..]


-- Four-bit signed integers: -8, ..., 7
newtype Int4 = Int4 { unInt4 :: Int } deriving (Eq, Ord)

int4 :: Int -> Int4
int4 = Int4 . narrowS 4

instance Show Int4 where
  show = show . unInt4

instance Read Int4 where
  readsPrec = readsPrecNewtype int4

instance Num Int4 where
  (+) = oNewtype int4 unInt4 (+)
  (-) = oNewtype int4 unInt4 (-)
  (*) = oNewtype int4 unInt4 (*)
  abs = fNewtype int4 unInt4 abs
  signum = fNewtype int4 unInt4 signum
  fromInteger = int4 . fromInteger

instance Real Int4 where
  toRational (Int4 x) = fromIntegral x % 1

instance Integral Int4 where
  quotRem = otNewtype int4 unInt4 quotRem
  toInteger = toInteger . unInt4

instance Bounded Int4 where
  maxBound = Int4 7
  minBound = Int4 (-8)

instance Enum Int4 where
  toEnum   = int4
  fromEnum = unInt4
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Int4 where
  list = listIntegral


-- Four-bit unsigned integer: 0, ..., 15
newtype Word4 = Word4 { unWord4 :: Int } deriving (Eq, Ord)

word4 :: Int -> Word4
word4 = Word4 . narrowU 4

instance Show Word4 where
  show = show . unWord4

instance Read Word4 where
  readsPrec = readsPrecNewtype Word4

instance Num Word4 where
  (+) = oNewtype word4 unWord4 (+)
  (-) = oNewtype word4 unWord4 (-)
  (*) = oNewtype word4 unWord4 (*)
  abs = fNewtype word4 unWord4 abs
  signum = fNewtype word4 unWord4 signum
  fromInteger = word4 . fromInteger

instance Real Word4 where
  toRational (Word4 x) = fromIntegral x % 1

instance Integral Word4 where
  quotRem = otNewtype word4 unWord4 quotRem
  toInteger = toInteger . unWord4

instance Bounded Word4 where
  maxBound = Word4 15
  minBound = Word4 0

instance Enum Word4 where
  toEnum   = word4
  fromEnum = unWord4
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Word4 where
  list = [0..]


-- Natural numbers (including 0)
newtype Nat = Nat { unNat :: Int } deriving (Eq, Ord)

instance Show Nat where
  show (Nat x) = show x

instance Read Nat where
  readsPrec = readsPrecNewtype Nat

instance Num Nat where
  (+) = oNewtype Nat unNat (+)
  (-) = oNewtype Nat unNat (-)
  (*) = oNewtype Nat unNat (*)
  abs = fNewtype Nat unNat abs
  signum = fNewtype Nat unNat signum
  fromInteger = Nat . fromInteger

instance Real Nat where
  toRational (Nat x) = fromIntegral x % 1

instance Integral Nat where
  quotRem = otNewtype Nat unNat quotRem
  toInteger = toInteger . unNat

instance Enum Nat where
  toEnum   = Nat
  fromEnum = unNat
  enumFrom (Nat x) = map Nat [x..]
  enumFromThen (Nat x) (Nat s) = map Nat [x,s..]

instance Listable Nat where
  list = [0..]

type UInt1 = Word1
type UInt2 = Word2
type UInt3 = Word3
type UInt4 = Word4
