-- |
-- Module      : Test.LeanCheck.Function.ShowFunction
-- Copyright   : (c) 2015-2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of LeanCheck,
-- a simple enumerative property-based testing library.
--
-- This module exports the 'ShowFunction' typeclass,
-- its instances and related functions.
--
-- Using this module, it is possible to implement
-- a Show instance for functions:
--
-- > import Test.LeanCheck.ShowFunction
-- > instance (Show a, Listable a, ShowFunction b) => Show (a->b) where
-- >   show = showFunction 8
--
-- This shows functions as a case pattern with up to 8 cases.
--
-- The module
-- @Test.LeanCheck.Function.Show@ ('Test.LeanCheck.Function.Show')
-- exports an instance like the one above.
module Test.LeanCheck.Function.ShowFunction
  ( showFunction
  , showFunctionLine
  , Binding
  , bindings
  , clarifiedBindings
  , clarifyBindings
  , simplifyBindings
  , ShowFunction (..)
  , tBindingsShow
  -- * Re-exports
  , Listable
  , name
  )
where

import Test.LeanCheck.Core
import Test.LeanCheck.Error (errorToNothing)
import Test.LeanCheck.Utils.Types
import Test.LeanCheck.Stats (classifyOn)
import Data.List (intercalate, sortBy)
import Data.Maybe
import Data.Function (on)

-- | A functional binding in a showable format.
type Binding = ([String], Maybe String)

-- | 'ShowFunction' values are those for which
--   we can return a list of functional bindings.
--
-- As a user, you probably want 'showFunction' and 'showFunctionLine'.
--
-- Non functional instances should be defined by:
--
-- > instance ShowFunction Ty where tBindings = tBindingsShow
class ShowFunction a where
  tBindings :: a -> [[Binding]]

-- | Given a 'ShowFunction' value, return a list of bindings
--   for printing.  Examples:
--
-- > bindings True == [([],True)]
-- > bindings (id::Int) == [(["0"],"0"), (["1"],"1"), (["-1"],"-1"), ...
-- > bindings (&&) == [ (["False","False"], "False")
-- >                  , (["False","True"], "False")
-- >                  , (["True","False"], "False")
-- >                  , (["True","True"], "True")
-- >                  ]
bindings :: ShowFunction a => a -> [Binding]
bindings = concat . tBindings


-- instances for (algebraic/numeric) data types --
-- | A default implementation of tBindings for already 'Show'-able types.
tBindingsShow :: Show a => a -> [[Binding]]
tBindingsShow x = [[([],errorToNothing $ show x)]]

instance ShowFunction ()       where tBindings = tBindingsShow
instance ShowFunction Bool     where tBindings = tBindingsShow
instance ShowFunction Int      where tBindings = tBindingsShow
instance ShowFunction Integer  where tBindings = tBindingsShow
instance ShowFunction Char     where tBindings = tBindingsShow
instance ShowFunction Float    where tBindings = tBindingsShow
instance ShowFunction Double   where tBindings = tBindingsShow
instance ShowFunction Ordering where tBindings = tBindingsShow
instance Show a => ShowFunction [a]       where tBindings = tBindingsShow
instance Show a => ShowFunction (Maybe a) where tBindings = tBindingsShow
instance (Show a, Show b) => ShowFunction (Either a b) where tBindings = tBindingsShow
instance (Show a, Show b) => ShowFunction (a,b) where tBindings = tBindingsShow

-- instance for functional value type --
instance (Show a, Listable a, ShowFunction b) => ShowFunction (a->b) where
  tBindings f = concatMapT tBindingsFor tiers
    where tBindingsFor x = mapFst (show x:) `mapT` tBindings (f x)
          mapFst f (x,y) = (f x, y)

paren :: String -> String
paren s = "(" ++ s ++ ")"

varnamesFor :: ShowFunction a => a -> [String]
varnamesFor = zipWith const varnames . fst . head . bindings
  where varnames = ["x","y","z","w"] ++ map (++"'") varnames

showTuple :: [String] -> String
showTuple [x]  =  x
showTuple xs | all (== "_") xs  =  "_"
             | otherwise        =  paren $ intercalate "," xs

showNBindingsOf :: ShowFunction a => Int -> Int -> a -> [String]
showNBindingsOf m n f = take n bs
                     ++ ["..." | length bs' >= m || length bs > n]
  where bs' = clarifiedBindings m n f
        bs = [ showTuple as ++ " -> " ++ r
             | (as, Just r) <- bs' ]

isValue :: ShowFunction a => a -> Bool
isValue f = case bindings f of
              [([],_)] -> True
              _        -> False

showValueOf :: ShowFunction a => a -> String
showValueOf x = case snd . head . bindings $ x of
                  Nothing -> "undefined"
                  Just x' -> x'

-- | Given a number of patterns to show, shows a 'ShowFunction' value.
--
-- > showFunction undefined True == "True"
-- > showFunction 3 (id::Int) == "\\x -> case x of\n\
-- >                              \        0 -> 0\n\
-- >                              \        1 -> 1\n\
-- >                              \        -1 -> -1\n\
-- >                              \        ...\n"
-- > showFunction 4 (&&) == "\\x y -> case (x,y) of\n\
-- >                         \          (False,False) -> False\n\
-- >                         \          (False,True) -> False\n\
-- >                         \          (True,False) -> False\n\
-- >                         \          (True,True) -> True\n"
--
-- This can be used as an implementation of show for functions:
--
-- > instance (Show a, Listable a, ShowFunction b) => Show (a->b) where
-- >   show = showFunction 8
showFunction :: ShowFunction a => Int -> a -> String
showFunction n = showFunctionL False (n*n+1) n

-- | Same as showFunction, but has no line breaks.
--
-- > showFunction 2 (id::Int) == "\\x -> case x of 0 -> 0; 1 -> 1; ..."
showFunctionLine :: ShowFunction a => Int -> a -> String
showFunctionLine n = showFunctionL True (n*n+1) n

-- | isUndefined checks if a function is totally undefined
--   for the given maximum number of values
isUndefined :: ShowFunction a => Int -> a -> Bool
isUndefined m = all (isNothing . snd) . take m . bindings

-- | checks if a function is constant
--   for the given maximum number of values
isConstant :: ShowFunction a => Int -> a -> Bool
isConstant m f = case take m $ bindings f of
                 []          -> False -- uninhabited type?
                 ((_,r'):bs) -> all (\(_,r) -> r == r') bs

-- | shows a constant function
showConstant :: ShowFunction a => Int -> a -> String
showConstant m f = "\\" ++ unwords vs ++ " -> " ++ fromMaybe "undefined" r
  where
  (as,r) = head $ bindings f
  vs = replicate (length as) "_"

-- The first boolean parameter tells if we are showing
-- the function on a single line
showFunctionL :: ShowFunction a => Bool -> Int -> Int -> a -> String
showFunctionL singleLine m n f | isValue f = showValueOf f
showFunctionL singleLine m n f | isConstant m f = showConstant m f
showFunctionL singleLine m n f | otherwise = lambdaPat ++ caseExp
  where
    vs = varnamesFor f
    lambdaPat = "\\" ++ unwords vs ++ " -> "
    casePat = "case " ++ showTuple vs ++ " of"
    bs = showNBindingsOf m n f
    sep | singleLine = " "
        | otherwise = "\n"
    cases | singleLine = intercalate "; " bs
          | otherwise  = unlines
                       $ (replicate (length lambdaPat) ' ' ++) `map` bs
    caseExp = if isUndefined m f
                then "undefined"
                else casePat ++ sep ++ cases

-- instances for further tuple arities --
instance (Show a, Show b, Show c)
      => ShowFunction (a,b,c) where tBindings = tBindingsShow
instance (Show a, Show b, Show c, Show d)
      => ShowFunction (a,b,c,d) where tBindings = tBindingsShow
instance (Show a, Show b, Show c, Show d, Show e)
      => ShowFunction (a,b,c,d,e) where tBindings = tBindingsShow
instance (Show a, Show b, Show c, Show d, Show e, Show f)
      => ShowFunction (a,b,c,d,e,f) where tBindings = tBindingsShow
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g)
      => ShowFunction (a,b,c,d,e,f,g) where tBindings = tBindingsShow
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
      => ShowFunction (a,b,c,d,e,f,g,h) where tBindings = tBindingsShow

-- instance for types from Test.LeanCheck.Utils.Types
instance ShowFunction Nat   where tBindings = tBindingsShow
instance ShowFunction Nat1  where tBindings = tBindingsShow
instance ShowFunction Nat2  where tBindings = tBindingsShow
instance ShowFunction Nat3  where tBindings = tBindingsShow
instance ShowFunction Nat4  where tBindings = tBindingsShow
instance ShowFunction Nat5  where tBindings = tBindingsShow
instance ShowFunction Nat6  where tBindings = tBindingsShow
instance ShowFunction Nat7  where tBindings = tBindingsShow
instance ShowFunction Int1  where tBindings = tBindingsShow
instance ShowFunction Int2  where tBindings = tBindingsShow
instance ShowFunction Int3  where tBindings = tBindingsShow
instance ShowFunction Int4  where tBindings = tBindingsShow
instance ShowFunction Word1 where tBindings = tBindingsShow
instance ShowFunction Word2 where tBindings = tBindingsShow
instance ShowFunction Word3 where tBindings = tBindingsShow
instance ShowFunction Word4 where tBindings = tBindingsShow

-- | Hard coded function names and bindings
--   Search this list for a short name for your function.
functionNames :: [(String, [Binding])]
functionNames =
  [ "id"          `for` (id          :: () -> ())
  , "const"       `for` (const       :: () -> () -> ())

  , "id"          `for` (id          :: Bool -> Bool)
  , "not"         `for` (not         :: Bool -> Bool)
  , "const False" `for` (const False :: Bool -> Bool)
  , "const True"  `for` (const True  :: Bool -> Bool)
  , "const"       `for` (const :: Bool -> Bool -> Bool)

  , "(&&)" `for` (&&)
  , "(||)" `for` (||)

  , "id"      `for` (id      :: Int -> Int)
  , "const 0" `for` (const 0 :: Int -> Int)
  , "const 1" `for` (const 1 :: Int -> Int)
  , "abs"     `for` (abs     :: Int -> Int)
  , "negate"  `for` (negate  :: Int -> Int)
  , "(+)"     `for` ((+)     :: Int -> Int -> Int)
  , "(*)"     `for` ((*)     :: Int -> Int -> Int)
  , "(-)"     `for` ((-)     :: Int -> Int -> Int)
  , "const"   `for` (const   :: Int -> Int -> Int)
  , "odd"     `for` (odd     :: Int -> Bool)
  , "even"    `for` (even    :: Int -> Bool)
  ]
  where
  n `for` f = (n, bindings f)

-- | Tries to name a function heuristically
name :: ShowFunction a => Int -> a -> Maybe String
name n f = listToMaybe [ nm | (nm, bs) <- functionNames
                            , take n bs == take n (bindings f)]

generalizations :: [String] -> [[String]]
generalizations []     = [[]]
generalizations (v:vs) = map ("_":) gvs ++ map (v:) gvs
  where
  gvs = generalizations vs

-- Should be read as "is generalization of":
--
-- > > ["_","_","_"] ~> ["1","2","3"]
-- > True
-- > > ["1","2","3"] ~> ["_","_","_"]
-- > False
-- > > ["_","3"] ~> ["1","3"]
-- > True
-- > > ["_","3"] ~> ["_","4"]
-- > False
(~>) :: [String] -> [String] -> Bool
[]       ~> []      =  True
("_":ws) ~> (v:vs)  =  ws ~> vs
(w:ws)   ~> (v:vs)  =  w == v && ws ~> vs
_        ~> _       =  False

(<~) :: [String] -> [String] -> Bool
(<~) = flip (~>)

(<~~) :: Binding -> Binding -> Bool
(as,r) <~~ (as',r') = as <~ as' && r == r'

clarifiedBindings :: ShowFunction a => Int -> Int -> a -> [Binding]
clarifiedBindings m n f
  | length bs1 <= n  =  bs1
  | otherwise        =  bs0
  where
  bs0  =  take m $ bindings f
  bs1  =  clarifyBindings bs0

clarifyBindings :: [Binding] -> [Binding]
clarifyBindings bs = head $ sortOn length $
  [ bs
  , simplifyBindings bs
  , simplifyBindings . concat . sortOn length $ classifyOn snd bs
  ]

simplifyBindings :: [Binding] -> [Binding]
simplifyBindings = simplify []
  where
  simplify :: [Binding] -> [Binding] -> [Binding]
  simplify bs' []           =  reverse bs'
  simplify bs' ((as,r):bs)  =  simplify (bs''++bs') [b | b <- bs, none (b <~~) bs'']
    where
    bs'' = discardLater (<~~)
         [ (gas,r) | gas <- generalizations as
                   , and [r' == r | (as',r') <- bs, gas ~> as'] ]

-- general auxiliary functions

discard :: (a -> Bool) -> [a] -> [a]
discard p = filter (not . p)

discardLater :: (a -> a -> Bool) -> [a] -> [a]
discardLater (?>) = dl
  where
  dl []     = []
  dl (x:xs) = x : discard (?> x) (dl xs)

none :: (a -> Bool) -> [a] -> Bool
none p = not . any p

-- sortOn is only available on GHC > 7.8
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)
