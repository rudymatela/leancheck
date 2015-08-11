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
  , UInt1
  , UInt2
  , UInt3
  , UInt4
  , Nat(N,unN) -- TODO: Remove export of constructors
  )
where

import Test.Check (Listable(..),(\/))
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
newtype UInt1 = UInt1 { unUInt1 :: Int } deriving (Eq, Ord)

instance Show UInt1 where
  show = show . unUInt1

instance Read UInt1 where
  readsPrec = readsPrecNewtype UInt1

instance Num UInt1 where
  (+) = oNewtype UInt1 unUInt1 (+)
  (-) = oNewtype UInt1 unUInt1 (-)
  (*) = oNewtype UInt1 unUInt1 (*)
  abs = fNewtype UInt1 unUInt1 abs
  signum = fNewtype UInt1 unUInt1 signum
  fromInteger = eBoundedFromSomething "UInt1.fromInteger" (UInt1 . fromInteger)

instance Real UInt1 where
  toRational (UInt1 x) = fromIntegral x % 1

instance Integral UInt1 where
  quotRem = otNewtype UInt1 unUInt1 quotRem
  toInteger = toInteger . unUInt1

instance Bounded UInt1 where
  maxBound = UInt1 1
  minBound = UInt1 0

instance Enum UInt1 where
  toEnum   = eBoundedFromSomething "UInt1.toEnum" UInt1
  fromEnum = unUInt1
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable UInt1 where
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
  list = [0,-1..] \/ [1..]


-- Two-bit unsigned integer: 0, 1, 2, 3
newtype UInt2 = UInt2 { unUInt2 :: Int } deriving (Eq, Ord)

instance Show UInt2 where
  show = show . unUInt2

instance Read UInt2 where
  readsPrec = readsPrecNewtype UInt2

instance Num UInt2 where
  (+) = oNewtype UInt2 unUInt2 (+)
  (-) = oNewtype UInt2 unUInt2 (-)
  (*) = oNewtype UInt2 unUInt2 (*)
  abs = fNewtype UInt2 unUInt2 abs
  signum = fNewtype UInt2 unUInt2 signum
  fromInteger = eBoundedFromSomething "UInt2.fromInteger" (UInt2 . fromInteger)

instance Real UInt2 where
  toRational (UInt2 x) = fromIntegral x % 1

instance Integral UInt2 where
  quotRem = otNewtype UInt2 unUInt2 quotRem
  toInteger = toInteger . unUInt2

instance Bounded UInt2 where
  maxBound = UInt2 3
  minBound = UInt2 0

instance Enum UInt2 where
  toEnum   = eBoundedFromSomething "UInt2.toEnum" UInt2
  fromEnum = unUInt2
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable UInt2 where
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
  list = [0,-1..] \/ [1..]


-- Three-bit unsigned integer: 0, 1, 2, 3, 4, 5, 6, 7
newtype UInt3 = UInt3 { unUInt3 :: Int } deriving (Eq, Ord)

instance Show UInt3 where
  show = show . unUInt3

instance Read UInt3 where
  readsPrec = readsPrecNewtype UInt3

instance Num UInt3 where
  (+) = oNewtype UInt3 unUInt3 (+)
  (-) = oNewtype UInt3 unUInt3 (-)
  (*) = oNewtype UInt3 unUInt3 (*)
  abs = fNewtype UInt3 unUInt3 abs
  signum = fNewtype UInt3 unUInt3 signum
  fromInteger = eBoundedFromSomething "UInt3.fromInteger" (UInt3 . fromInteger)

instance Real UInt3 where
  toRational (UInt3 x) = fromIntegral x % 1

instance Integral UInt3 where
  quotRem = otNewtype UInt3 unUInt3 quotRem
  toInteger = toInteger . unUInt3

instance Bounded UInt3 where
  maxBound = UInt3 7
  minBound = UInt3 0

instance Enum UInt3 where
  toEnum   = eBoundedFromSomething "UInt3.toEnum" UInt3
  fromEnum = unUInt3
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable UInt3 where
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
  list = [0,-1..] \/ [1..]


-- Four-bit unsigned integer: 0, ..., 15
newtype UInt4 = UInt4 { unUInt4 :: Int } deriving (Eq, Ord)

instance Show UInt4 where
  show = show . unUInt4

instance Read UInt4 where
  readsPrec = readsPrecNewtype UInt4

instance Num UInt4 where
  (+) = oNewtype UInt4 unUInt4 (+)
  (-) = oNewtype UInt4 unUInt4 (-)
  (*) = oNewtype UInt4 unUInt4 (*)
  abs = fNewtype UInt4 unUInt4 abs
  signum = fNewtype UInt4 unUInt4 signum
  fromInteger = eBoundedFromSomething "UInt4.fromInteger" (UInt4 . fromInteger)

instance Real UInt4 where
  toRational (UInt4 x) = fromIntegral x % 1

instance Integral UInt4 where
  quotRem = otNewtype UInt4 unUInt4 quotRem
  toInteger = toInteger . unUInt4

instance Bounded UInt4 where
  maxBound = UInt4 15
  minBound = UInt4 0

instance Enum UInt4 where
  toEnum   = eBoundedFromSomething "UInt4.toEnum" UInt4
  fromEnum = unUInt4
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Listable UInt4 where
  list = [0..]


-- Natural numbers (including 0)
newtype Nat = N { unN :: Int }
  deriving (Eq, Ord)

instance Listable Nat where
  list = map N [0,1..]

instance Show Nat where
  show (N x) = show x
