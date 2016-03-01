-- | Infix operators for type binding via dummy values
--
-- *                    as type of: '(-:)'
-- *           argument as type of: '(-:>)'
-- *             result as type of: '(->:)'
-- * argument of result as type of: '(->:>)'
-- *   result of result as type of: '(->>:)'
-- * argument of result of result as type of: '(->>:>)'
-- *   result of result of result as type of: '(->>>:)'
module Test.TypeBinding where

import Test.Types

undefinedOf :: String -> a
undefinedOf fn = error $ "Test.TypeBinding." ++ fn

-- | Type restricted version of const
-- that forces its first argument
-- to have the same type as the second.
--
-- > > :t 10 -: int
-- > Int
-- > > :t undefined -: ()
-- > ()
-- > > :t undefined -: 'a' >- 'b'
-- > Char -> Char
(-:) :: a -> a -> a
(-:) = asTypeOf -- const
infixl 1 -:

-- | Type restricted version of const
-- that forces the argument of its first argument
-- to have the same type as the second.
--
-- > f ->: int == f -: int >- und
(-:>) :: (a -> b) -> a -> (a -> b)
(-:>) = const
infixl 1 -:>

-- | Type restricted version of const
-- that forces the argument of the result of its first argument
-- to have the same type as the second.
--
-- > f ->>: int == f -: und >- int >- und
(->:>) :: (a -> b -> c) -> b -> (a -> b -> c)
(->:>) = const
infixl 1 ->:>

-- | Type restricted version of const
-- that forces the argument of the result of the result of its first argument
-- to have the same type as the second.
(->>:>) :: (a -> b -> c -> d) -> c -> (a -> b -> c -> d)
(->>:>) = const
infixl 1 ->>:>

-- Returns an undefined functional value
-- that takes an argument of the type of its first argument
-- and return a value of the type of its second argument.
--
-- > > :t 'a' >- 'b'
-- > Char -> Char
-- > > :t int >- bool >- int
-- > Int -> Bool -> Int
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
may :: a -> Maybe a
may = undefinedOf "may"

either :: a -> b -> Either a b
either = undefinedOf "either"


-- Dummy values of Test.Types's types:

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
