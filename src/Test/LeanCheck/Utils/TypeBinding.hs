-- |
-- Module      : Test.LeanCheck.Utils.TypeBinding
-- Copyright   : (c) 2015-2017 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- Infix operators for type binding using dummy first-class values.
--
-- Those are useful when property based testing to avoid repetition.
-- Suppose:
--
-- > prop_sortAppend :: Ord a => [a] -> Bool
-- > prop_sortAppend xs =  sort (xs++ys) == sort (ys++xs)
--
-- Then this:
--
-- > testResults n =
-- >   [ holds n (prop_sortAppend :: [Int] -> [Int] -> Bool)
-- >   , holds n (prop_sortAppend :: [UInt2] -> [UInt2] -> Bool)
-- >   , holds n (prop_sortAppend :: [Bool] -> [Bool] -> Bool)
-- >   , holds n (prop_sortAppend :: [Char] -> [Char] -> Bool)
-- >   , holds n (prop_sortAppend :: [String] -> [String] -> Bool)
-- >   , holds n (prop_sortAppend :: [()] -> [()] -> Bool)
-- >   ]
--
-- Becomes this:
--
-- > testResults n =
-- >   [ holds n $ prop_sortAppend -:> [int]
-- >   , holds n $ prop_sortAppend -:> [uint2]
-- >   , holds n $ prop_sortAppend -:> [bool]
-- >   , holds n $ prop_sortAppend -:> [char]
-- >   , holds n $ prop_sortAppend -:> [string]
-- >   , holds n $ prop_sortAppend -:> [()]
-- >   ]
--
-- Or even:
--
-- > testResults n = concat
-- >   [ for int, for uint2, for bool, for (), for char, for string ]
-- >   where for a = [ holds n $ prop_sortAppend -:> a ]
--
-- This last form is useful when testing multiple properties for multiple
-- types.
module Test.LeanCheck.Utils.TypeBinding
  (
  -- * Type binding operators
  --
  -- | Summary:
  --
  -- *                 as type of: '-:'
  -- *        argument as type of: '-:>'
  -- *          result as type of: '->:'
  -- * second argument as type of: '->:>'
  -- * second  result  as type of: '->>:'
  -- * third  argument as type of: '->>:>'
  -- * third   result  as type of: '->>>:'
    (-:)
  , (-:>)
  , (->:)
  , (->:>)
  , (->>:)
  , (->>:>)
  , (->>>:)

  -- * Dummy (undefined) values
  -- ** Standard Haskell types
  , und
  , (>-)
  , bool
  , int, integer
  , float, double
  , char, string
  , mayb, eith
  -- ** Testing types
  , nat
  , int1, uint1
  , int2, uint2
  , int3, uint3
  , int4, uint4
  )
where

import Test.LeanCheck.Utils.Types

undefinedOf :: String -> a
undefinedOf fn = error $ "Test.LeanCheck.TypeBinding." ++ fn

-- | Type restricted version of const
-- that forces its first argument
-- to have the same type as the second.
-- A symnonym to 'asTypeOf':
--
-- >  value -: ty  =  value :: Ty
--
-- Examples:
--
-- >  10 -: int   =  10 :: Int
-- >  undefined -: 'a' >- 'b'  =  undefined :: Char -> Char
(-:) :: a -> a -> a
(-:) = asTypeOf -- const
infixl 1 -:

-- | Type restricted version of const
-- that forces the argument of its first argument
-- to have the same type as the second:
--
-- >  f -:> ty  =  f -: ty >- und  =  f :: Ty -> a
--
-- Example:
--
-- >  abs -:> int   =  abs -: int >- und  =  abs :: Int -> Int
(-:>) :: (a -> b) -> a -> (a -> b)
(-:>) = const
infixl 1 -:>

-- | Type restricted version of const
-- that forces the result of its first argument
-- to have the same type as the second.
--
-- >  f ->: ty  =  f -: und >- ty  =  f :: a -> Ty
(->:) :: (a -> b) -> b -> (a -> b)
(->:) = const
infixl 1 ->:

-- | Type restricted version of const
-- that forces the second argument of its first argument
-- to have the same type as the second.
--
-- > f ->:> ty   =  f -: und -> ty -> und  =  f :: a -> Ty -> b
(->:>) :: (a -> b -> c) -> b -> (a -> b -> c)
(->:>) = const
infixl 1 ->:>

-- | Type restricted version of const
-- that forces the result of the result of its first argument
-- to have the same type as the second.
--
-- > f ->>: ty   =  f -: und -> und -> ty  =  f :: a -> b -> Ty
(->>:) :: (a -> b -> c) -> c -> (a -> b -> c)
(->>:) = const
infixl 1 ->>:

-- | Type restricted version of const
-- that forces the third argument of its first argument
-- to have the same type as the second.
(->>:>) :: (a -> b -> c -> d) -> c -> (a -> b -> c -> d)
(->>:>) = const
infixl 1 ->>:>

-- | Type restricted version of const
-- that forces the result of the result of the result of its first argument
-- to have the same type as the second.
(->>>:) :: (a -> b -> c -> d) -> d -> (a -> b -> c -> d)
(->>>:) = const
infixl 1 ->>>:

-- | Returns an undefined functional value
-- that takes an argument of the type of its first argument
-- and return a value of the type of its second argument.
--
-- > ty >- ty  =  (undefined :: Ty -> Ty)
--
-- Examples:
--
-- > 'a' >- 'b'  =  char >- char  =  (undefined :: Char -> Char)
-- > int >- bool >- int  =  undefined :: Int -> Bool -> Int
(>-) :: a -> b -> (a -> b)
(>-) = undefinedOf "(>-): undefined function -- using dummy value?"
infixr 9 >-


-- Dummy values of standard Haskell types

-- | Shorthand for undefined
und :: a
und = undefinedOf "und"

int :: Int
int = undefinedOf "int"

integer :: Integer
integer = undefinedOf "integer"

float :: Float
float = undefinedOf "float"

double :: Double
double = undefinedOf "double"

bool :: Bool
bool = undefinedOf "bool"

char :: Char
char = undefinedOf "char"

string :: String
string = undefinedOf "string"

-- | It might be better to just use 'Just'
mayb :: a -> Maybe a
mayb = undefinedOf "mayb"

eith :: a -> b -> Either a b
eith = undefinedOf "eith"


-- Dummy values of Test.LeanCheck.Types's types:

nat :: Nat
nat = undefinedOf "nat"

int1 :: Int1
int1 = undefinedOf "int1"

int2 :: Int2
int2 = undefinedOf "int2"

int3 :: Int3
int3 = undefinedOf "int3"

int4 :: Int4
int4 = undefinedOf "int4"

uint1 :: UInt1
uint1 = undefinedOf "uint1"

uint2 :: UInt2
uint2 = undefinedOf "uint2"

uint3 :: UInt3
uint3 = undefinedOf "uint3"

uint4 :: UInt4
uint4 = undefinedOf "uint4"
