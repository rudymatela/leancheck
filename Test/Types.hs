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

eBoundedFromSomething :: (Show a, Ord a, Bounded a) => String -> (b -> a) -> b -> a
eBoundedFromSomething caller con n
    | x > maxBound || x < minBound = error
                                   $ "Test.Types.boundedToEnum("
                                  ++ caller ++ "): out of bounds"
    | otherwise = x
  where x = con n

boundedFromSomething :: (Show a, Ord a, Bounded a) => (b -> a) -> b -> a
boundedFromSomething = eBoundedFromSomething "??"

boundedEnumFrom :: (Ord a,Bounded a,Enum a) => a -> [a]
boundedEnumFrom x = [x..maxBound]

boundedEnumFromThen :: (Ord a,Bounded a,Enum a) => a -> a -> [a]
boundedEnumFromThen x y | x > y     = [x,y..minBound]
                        | otherwise = [x,y..maxBound]


-- Single-bit signed integer: -1, 0
newtype Int1 = Int1 { unInt1 :: Int } deriving (Eq, Ord)

instance Show Int1 where
  show = show . unInt1

instance Read Int1 where
  readsPrec = readsPrecNewtype Int1

instance Num Int1 where
  (+) = oNewtype Int1 unInt1 (+)
  (-) = oNewtype Int1 unInt1 (-)
  (*) = oNewtype Int1 unInt1 (*)
  abs = fNewtype Int1 unInt1 abs
  signum = fNewtype Int1 unInt1 signum
  fromInteger = eBoundedFromSomething "Int1.fromInteger" (Int1 . fromInteger)

instance Real Int1 where
  toRational (Int1 x) = fromIntegral x % 1

instance Integral Int1 where
  quotRem = otNewtype Int1 unInt1 quotRem
  toInteger = toInteger . unInt1

instance Bounded Int1 where
  maxBound = Int1 0
  minBound = Int1 (-1)

instance Enum Int1 where
  toEnum   = eBoundedFromSomething "Int1.toEnum" Int1
  fromEnum = unInt1
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Int1 where
  list = [0,minBound]


-- Single-bit unsigned integer: 0, 1
newtype Word1 = Word1 { unWord1 :: Int } deriving (Eq, Ord)

instance Show Word1 where
  show = show . unWord1

instance Read Word1 where
  readsPrec = readsPrecNewtype Word1

instance Num Word1 where
  (+) = oNewtype Word1 unWord1 (+)
  (-) = oNewtype Word1 unWord1 (-)
  (*) = oNewtype Word1 unWord1 (*)
  abs = fNewtype Word1 unWord1 abs
  signum = fNewtype Word1 unWord1 signum
  fromInteger = eBoundedFromSomething "Word1.fromInteger" (Word1 . fromInteger)

instance Real Word1 where
  toRational (Word1 x) = fromIntegral x % 1

instance Integral Word1 where
  quotRem = otNewtype Word1 unWord1 quotRem
  toInteger = toInteger . unWord1

instance Bounded Word1 where
  maxBound = Word1 1
  minBound = Word1 0

instance Enum Word1 where
  toEnum   = eBoundedFromSomething "Word1.toEnum" Word1
  fromEnum = unWord1
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Word1 where
  list = [0,maxBound]


-- Two-bit signed integer: -2, -1, 0, 1
newtype Int2 = Int2 { unInt2 :: Int } deriving (Eq, Ord)

instance Show Int2 where
  show = show . unInt2

instance Read Int2 where
  readsPrec = readsPrecNewtype Int2

instance Num Int2 where
  (+) = oNewtype Int2 unInt2 (+)
  (-) = oNewtype Int2 unInt2 (-)
  (*) = oNewtype Int2 unInt2 (*)
  abs = fNewtype Int2 unInt2 abs
  signum = fNewtype Int2 unInt2 signum
  fromInteger = eBoundedFromSomething "Int2.fromInteger" (Int2 . fromInteger)

instance Real Int2 where
  toRational (Int2 x) = fromIntegral x % 1

instance Integral Int2 where
  quotRem = otNewtype Int2 unInt2 quotRem
  toInteger = toInteger . unInt2

instance Bounded Int2 where
  maxBound = Int2 1
  minBound = Int2 (-2)

instance Enum Int2 where
  toEnum   = eBoundedFromSomething "Int2.toEnum" Int2
  fromEnum = unInt2
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Int2 where
  list = listIntegral


-- Two-bit unsigned integer: 0, 1, 2, 3
newtype Word2 = Word2 { unWord2 :: Int } deriving (Eq, Ord)

instance Show Word2 where
  show = show . unWord2

instance Read Word2 where
  readsPrec = readsPrecNewtype Word2

instance Num Word2 where
  (+) = oNewtype Word2 unWord2 (+)
  (-) = oNewtype Word2 unWord2 (-)
  (*) = oNewtype Word2 unWord2 (*)
  abs = fNewtype Word2 unWord2 abs
  signum = fNewtype Word2 unWord2 signum
  fromInteger = eBoundedFromSomething "Word2.fromInteger" (Word2 . fromInteger)

instance Real Word2 where
  toRational (Word2 x) = fromIntegral x % 1

instance Integral Word2 where
  quotRem = otNewtype Word2 unWord2 quotRem
  toInteger = toInteger . unWord2

instance Bounded Word2 where
  maxBound = Word2 3
  minBound = Word2 0

instance Enum Word2 where
  toEnum   = eBoundedFromSomething "Word2.toEnum" Word2
  fromEnum = unWord2
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Word2 where
  list = [0..]


-- Three-bit signed integer: -4, -3, -2, -1, 0, 1, 2, 3
newtype Int3 = Int3 { unInt3 :: Int } deriving (Eq, Ord)

instance Show Int3 where
  show = show . unInt3

instance Read Int3 where
  readsPrec = readsPrecNewtype Int3

instance Num Int3 where
  (+) = oNewtype Int3 unInt3 (+)
  (-) = oNewtype Int3 unInt3 (-)
  (*) = oNewtype Int3 unInt3 (*)
  abs = fNewtype Int3 unInt3 abs
  signum = fNewtype Int3 unInt3 signum
  fromInteger = eBoundedFromSomething "Int3.fromInteger" (Int3 . fromInteger)

instance Real Int3 where
  toRational (Int3 x) = fromIntegral x % 1

instance Integral Int3 where
  quotRem = otNewtype Int3 unInt3 quotRem
  toInteger = toInteger . unInt3

instance Bounded Int3 where
  maxBound = Int3 3
  minBound = Int3 (-4)

instance Enum Int3 where
  toEnum   = eBoundedFromSomething "Int3.toEnum" Int3
  fromEnum = unInt3
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Int3 where
  list = listIntegral


-- Three-bit unsigned integer: 0, 1, 2, 3, 4, 5, 6, 7
newtype Word3 = Word3 { unWord3 :: Int } deriving (Eq, Ord)

instance Show Word3 where
  show = show . unWord3

instance Read Word3 where
  readsPrec = readsPrecNewtype Word3

instance Num Word3 where
  (+) = oNewtype Word3 unWord3 (+)
  (-) = oNewtype Word3 unWord3 (-)
  (*) = oNewtype Word3 unWord3 (*)
  abs = fNewtype Word3 unWord3 abs
  signum = fNewtype Word3 unWord3 signum
  fromInteger = eBoundedFromSomething "Word3.fromInteger" (Word3 . fromInteger)

instance Real Word3 where
  toRational (Word3 x) = fromIntegral x % 1

instance Integral Word3 where
  quotRem = otNewtype Word3 unWord3 quotRem
  toInteger = toInteger . unWord3

instance Bounded Word3 where
  maxBound = Word3 7
  minBound = Word3 0

instance Enum Word3 where
  toEnum   = eBoundedFromSomething "Word3.toEnum" Word3
  fromEnum = unWord3
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Word3 where
  list = [0..]


-- Four-bit signed integers: -8, ..., 7
newtype Int4 = Int4 { unInt4 :: Int } deriving (Eq, Ord)

instance Show Int4 where
  show = show . unInt4

instance Read Int4 where
  readsPrec = readsPrecNewtype Int4

instance Num Int4 where
  (+) = oNewtype Int4 unInt4 (+)
  (-) = oNewtype Int4 unInt4 (-)
  (*) = oNewtype Int4 unInt4 (*)
  abs = fNewtype Int4 unInt4 abs
  signum = fNewtype Int4 unInt4 signum
  fromInteger = eBoundedFromSomething "Int4.fromInteger" (Int4 . fromInteger)

instance Real Int4 where
  toRational (Int4 x) = fromIntegral x % 1

instance Integral Int4 where
  quotRem = otNewtype Int4 unInt4 quotRem
  toInteger = toInteger . unInt4

instance Bounded Int4 where
  maxBound = Int4 7
  minBound = Int4 (-8)

instance Enum Int4 where
  toEnum   = eBoundedFromSomething "Int4.toEnum" Int4
  fromEnum = unInt4
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable Int4 where
  list = listIntegral


-- Four-bit unsigned integer: 0, ..., 15
newtype Word4 = Word4 { unWord4 :: Int } deriving (Eq, Ord)

instance Show Word4 where
  show = show . unWord4

instance Read Word4 where
  readsPrec = readsPrecNewtype Word4

instance Num Word4 where
  (+) = oNewtype Word4 unWord4 (+)
  (-) = oNewtype Word4 unWord4 (-)
  (*) = oNewtype Word4 unWord4 (*)
  abs = fNewtype Word4 unWord4 abs
  signum = fNewtype Word4 unWord4 signum
  fromInteger = eBoundedFromSomething "Word4.fromInteger" (Word4 . fromInteger)

instance Real Word4 where
  toRational (Word4 x) = fromIntegral x % 1

instance Integral Word4 where
  quotRem = otNewtype Word4 unWord4 quotRem
  toInteger = toInteger . unWord4

instance Bounded Word4 where
  maxBound = Word4 15
  minBound = Word4 0

instance Enum Word4 where
  toEnum   = eBoundedFromSomething "Word4.toEnum" Word4
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
