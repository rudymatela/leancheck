-- |
-- Module      : Test.LeanCheck.Utils.TypeBinding
-- Copyright   : (c) 2015-2020 Rudy Matela
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
-- > testResults n  =  concat
-- >   [ for int, for uint2, for bool, for (), for char, for string ]
-- >   where  for a  =  [ holds n $ prop_sortAppend -:> a ]
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
  , (->>>:>)
  , (->>>>:)
  , (->>>>:>)
  , (->>>>>:)
  , (->>>>>:>)
  , (->>>>>>:)
  , (->>>>>>:>)
  , (->>>>>>>:)
  , (->>>>>>>:>)
  , (->>>>>>>>:)
  , (->>>>>>>>:>)
  , (->>>>>>>>>:)
  , (->>>>>>>>>:>)
  , (->>>>>>>>>>:)
  , (->>>>>>>>>>:>)
  , (->>>>>>>>>>>:)
  , (->>>>>>>>>>>:>)
  , (->>>>>>>>>>>>:)

  -- * Dummy (undefined) values
  -- ** Standard Haskell types
  , und
  , (>-)
  , bool
  , int, integer
  , float, double
  , rational
  , char, string
  , ordering
  , mayb, eith
  -- ** Testing types
  , natural
  , nat
  , int1
  , int2
  , int3
  , int4
  , word1
  , word2
  , word3
  , word4
  -- *** Deprecated testing types
  , uint1
  , uint2
  , uint3
  , uint4
  )
where

import Test.LeanCheck.Utils.Types

undefinedOf :: String -> a
undefinedOf fn  =  error $ "Test.LeanCheck.TypeBinding." ++ fn

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
(-:)  =  asTypeOf  -- const
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
(-:>)  =  const
infixl 1 -:>

-- | Type restricted version of const
-- that forces the result of its first argument
-- to have the same type as the second.
--
-- >  f ->: ty  =  f -: und >- ty  =  f :: a -> Ty
(->:) :: (a -> b) -> b -> (a -> b)
(->:)  =  const
infixl 1 ->:

-- | Type restricted version of const
-- that forces the second argument of its first argument
-- to have the same type as the second.
--
-- > f ->:> ty   =  f -: und -> ty -> und  =  f :: a -> Ty -> b
(->:>) :: (a -> b -> c) -> b -> (a -> b -> c)
(->:>)  =  const
infixl 1 ->:>

-- | Type restricted version of const
-- that forces the result of the result of its first argument
-- to have the same type as the second.
--
-- > f ->>: ty   =  f -: und -> und -> ty  =  f :: a -> b -> Ty
(->>:) :: (a -> b -> c) -> c -> (a -> b -> c)
(->>:)  =  const
infixl 1 ->>:

-- | Type restricted version of const
-- that forces the third argument of its first argument
-- to have the same type as the second.
(->>:>) :: (a -> b -> c -> d) -> c -> (a -> b -> c -> d)
(->>:>)  =  const
infixl 1 ->>:>

-- | Type restricted version of const
-- that forces the result of the result of the result of its first argument
-- to have the same type as the second.
(->>>:) :: (a -> b -> c -> d) -> d -> (a -> b -> c -> d)
(->>>:)  =  const
infixl 1 ->>>:

-- | Forces the 4th argument type.
(->>>:>) :: (a -> b -> c -> d -> e) -> d -> (a -> b -> c -> d -> e)
(->>>:>)  =  const
infixl 1 ->>>:>

-- | Forces the result type of a 4-argument function.
(->>>>:) :: (a -> b -> c -> d -> e) -> e -> (a -> b -> c -> d -> e)
(->>>>:)  =  const
infixl 1 ->>>>:

-- | Forces the 5th argument type.
(->>>>:>) :: (a -> b -> c -> d -> e -> f) -> e -> (a -> b -> c -> d -> e -> f)
(->>>>:>)  =  const
infixl 1 ->>>>:>

-- | Forces the result type of a 5-argument function.
(->>>>>:) :: (a -> b -> c -> d -> e -> f) -> f -> (a -> b -> c -> d -> e -> f)
(->>>>>:)  =  const
infixl 1 ->>>>>:

-- | Forces the 6th argument type.
(->>>>>:>) :: (a->b->c->d->e->f->g) -> f -> (a->b->c->d->e->f->g)
(->>>>>:>)  =  const
infixl 1 ->>>>>:>

-- | Forces the result type of a 6-argument function.
(->>>>>>:) :: (a->b->c->d->e->f->g) -> g -> (a->b->c->d->e->f->g)
(->>>>>>:)  =  const
infixl 1 ->>>>>>:

-- | Forces the 7th argument type.
(->>>>>>:>) :: (a->b->c->d->e->f->g->h) -> g -> (a->b->c->d->e->f->g->h)
(->>>>>>:>)  =  const
infixl 1 ->>>>>>:>

-- | Forces the result type of a 7-argument function.
(->>>>>>>:) :: (a->b->c->d->e->f->g->h) -> h -> (a->b->c->d->e->f->g->h)
(->>>>>>>:)  =  const
infixl 1 ->>>>>>>:

-- | Forces the 8th argument type.
(->>>>>>>:>) :: (a->b->c->d->e->f->g->h->i) -> h -> (a->b->c->d->e->f->g->h->i)
(->>>>>>>:>)  =  const
infixl 1 ->>>>>>>:>

-- | Forces the result type of a 8-argument function.
(->>>>>>>>:) :: (a->b->c->d->e->f->g->h->i) -> i -> (a->b->c->d->e->f->g->h->i)
(->>>>>>>>:)  =  const
infixl 1 ->>>>>>>>:

-- | Forces the 9th argument type.
(->>>>>>>>:>) :: (a->b->c->d->e->f->g->h->i->j) -> i -> (a->b->c->d->e->f->g->h->i->j)
(->>>>>>>>:>)  =  const
infixl 1 ->>>>>>>>:>

-- | Forces the result type of a 9-argument function.
(->>>>>>>>>:) :: (a->b->c->d->e->f->g->h->i->j) -> j -> (a->b->c->d->e->f->g->h->i->j)
(->>>>>>>>>:)  =  const
infixl 1 ->>>>>>>>>:

-- | Forces the type of the 10th argument.
(->>>>>>>>>:>) :: (a->b->c->d->e->f->g->h->i->j->k) -> j -> (a->b->c->d->e->f->g->h->i->j->k)
(->>>>>>>>>:>)  =  const
infixl 1 ->>>>>>>>>:>

-- | Forces the result type of a 10-argument function.
(->>>>>>>>>>:) :: (a->b->c->d->e->f->g->h->i->j->k) -> k -> (a->b->c->d->e->f->g->h->i->j->k)
(->>>>>>>>>>:)  =  const
infixl 1 ->>>>>>>>>>:

-- | Forces the type of the 11th argument.
(->>>>>>>>>>:>) :: (a->b->c->d->e->f->g->h->i->j->k->l) -> k -> (a->b->c->d->e->f->g->h->i->j->k->l)
(->>>>>>>>>>:>)  =  const
infixl 1 ->>>>>>>>>>:>

-- | Forces the result type of a 11-argument function.
(->>>>>>>>>>>:) :: (a->b->c->d->e->f->g->h->i->j->k->l) -> l -> (a->b->c->d->e->f->g->h->i->j->k->l)
(->>>>>>>>>>>:)  =  const
infixl 1 ->>>>>>>>>>>:

-- | Forces the type of the 12th argument.
(->>>>>>>>>>>:>) :: (a->b->c->d->e->f->g->h->i->j->k->l->m) -> m -> (a->b->c->d->e->f->g->h->i->j->k->l->m)
(->>>>>>>>>>>:>)  =  const
infixl 1 ->>>>>>>>>>>:>

-- | Forces the result type of a 12-argument function.
(->>>>>>>>>>>>:) :: (a->b->c->d->e->f->g->h->i->j->k->l->m) -> m -> (a->b->c->d->e->f->g->h->i->j->k->l->m)
(->>>>>>>>>>>>:)  =  const
infixl 1 ->>>>>>>>>>>>:

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
(>-)  =  undefinedOf "(>-): undefined function -- using dummy value?"
infixr 9 >-


-- Dummy values of standard Haskell types

-- | Shorthand for undefined
und :: a
und  =  undefinedOf "und"

-- | Undefined 'Int' value for use with type binding operators.
--
-- > check $ (\x y -> x + y == y + x) ->:> int
int :: Int
int  =  undefinedOf "int"

-- | Undefined 'Integer' value for use with type binding operators.
--
-- > check $ (\x y -> x + y == y + x) ->:> integer
integer :: Integer
integer  =  undefinedOf "integer"

-- | Undefined 'Float' value for use with type binding operators.
float :: Float
float  =  undefinedOf "float"

-- | Undefined 'Double' value for use with type binding operators.
double :: Double
double  =  undefinedOf "double"

-- | Undefined 'Rational' value for use with type binding operators.
rational :: Rational
rational  =  undefinedOf "rational"

-- | Undefined 'Bool' value.
bool :: Bool
bool  =  undefinedOf "bool"

-- | Undefined 'Char' value.
char :: Char
char  =  undefinedOf "char"

-- | Undefined 'String' value.
string :: String
string  =  undefinedOf "string"

-- | Undefined 'Ordering' value.
ordering :: Ordering
ordering  =  undefinedOf "ordering"

-- | Undefined 'Maybe' value.  Uses the type of the given value as the argument
--   type.  For use with type binding operators.
--
-- To check a property with the first argument bound to 'Maybe' 'Int', do:
--
-- > check $ prop -:> mayb int
mayb :: a -> Maybe a
mayb  =  undefinedOf "mayb"

-- | Undefined 'Either' value.  Uses the types of the given values as the
--   argument types.  For use with type binding operators.
eith :: a -> b -> Either a b
eith  =  undefinedOf "eith"


-- Dummy values of Test.LeanCheck.Types's types:

-- | Undefined 'Natural' value.
natural :: Natural
natural  =  undefinedOf "natural"

-- | Undefined 'Nat' value.
nat :: Nat
nat  =  undefinedOf "nat"

-- | Undefined 'Int1' value.
int1 :: Int1
int1  =  undefinedOf "int1"

-- | Undefined 'Int2' value.
int2 :: Int2
int2  =  undefinedOf "int2"

-- | Undefined 'Int3' value.
int3 :: Int3
int3  =  undefinedOf "int3"

-- | Undefined 'Int4' value.
int4 :: Int4
int4  =  undefinedOf "int4"

-- | Undefined 'Word1' value.
word1 :: Word1
word1  =  undefinedOf "word1"

-- | Undefined 'Word2' value.
word2 :: Word2
word2  =  undefinedOf "word2"

-- | Undefined 'Word3' value.
word3 :: Word3
word3  =  undefinedOf "word3"

-- | Undefined 'Word4' value.
word4 :: Word4
word4  =  undefinedOf "word4"

-- | Deprecated.  Use 'word1'.
uint1 :: UInt1
uint1  =  undefinedOf "uint1"

-- | Deprecated.  Use 'word2'.
uint2 :: UInt2
uint2  =  undefinedOf "uint2"

-- | Deprecated.  Use 'word3'.
uint3 :: UInt3
uint3  =  undefinedOf "uint3"

-- | Deprecated.  Use 'word4'.
uint4 :: UInt4
uint4  =  undefinedOf "uint4"
