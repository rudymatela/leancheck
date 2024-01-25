-- |
-- Module      : Test.LeanCheck.Function.ShowFunction
-- Copyright   : (c) 2015-2020 Rudy Matela
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
-- a 'Show' instance for functions:
--
-- > import Test.LeanCheck.ShowFunction
-- > instance (Show a, Listable a, ShowFunction b) => Show (a->b) where
-- >   show  =  showFunction 8
--
-- This shows functions as a case pattern with up to 8 cases.
--
-- It will only work for functions whose ultimate return value is an instance
-- of 'ShowFunction'.  This module provides instances for most standard data
-- types ('Int', 'Bool', 'Maybe', ...).  Please see the 'ShowFunction'
-- typeclass documentation for how to declare istances for user-defined data
-- types.
--
-- The modules
-- "Test.LeanCheck.Function"
-- and
-- "Test.LeanCheck.Function.Show"
-- exports an instance like the one above.
{-# LANGUAGE CPP #-}
module Test.LeanCheck.Function.ShowFunction
  (
  -- * Showing functions
    showFunction
  , showFunctionLine

  -- * Support for user-defined algebraic datatypes on return values
  , ShowFunction (..)
  , bindtiersShow

  -- * Listing functional bindings
  , Binding
  , bindings

  -- * Pipeline for explaining, describing and clarifying bindings
  , explainedBindings
  , describedBindings
  , clarifiedBindings

  -- * Re-exports
  , Listable
  )
where

import Test.LeanCheck.Core
import Test.LeanCheck.Error (errorToNothing)
import Test.LeanCheck.Utils.Types
import Test.LeanCheck.Stats (classifyOn)
import Data.Maybe
import Data.Function (on)
import Data.Word
import Data.Int
import Data.Ratio
import Data.Complex
import Data.Char (GeneralCategory)
import System.Exit (ExitCode)
import System.IO (IOMode, BufferMode, SeekMode)
import Foreign.C
#ifndef __HUGS__
import Data.List (intercalate, sortBy, minimumBy)
#else
import Data.List (sortBy, minimumBy)

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss  =  concat (intersperse xs xss)
  where
  intersperse             :: a -> [a] -> [a]
  intersperse _   []      = []
  intersperse sep (x:xs)  = x : prependToAll sep xs
    where
    prependToAll :: a -> [a] -> [a]
    prependToAll _   []      =  []
    prependToAll sep (x:xs)  =  sep : x : prependToAll sep xs
#endif

-- | A functional binding in a showable format.
--   Argument values are represented as a list of strings.
--   The result value is represented by 'Just' a 'String' when defined
--   or by 'Nothing' when 'undefined'.
type Binding  =  ([String], Maybe String)

-- | 'ShowFunction' values are those for which
--   we can return a list of functional bindings.
--
-- Instances for 'show'able algebraic datatypes are defined using
-- 'bindtiersShow':
--
-- > instance ShowFunction Ty where bindtiers  =  bindtiersShow
class ShowFunction a where
  bindtiers :: a -> [[Binding]]

-- | Given a 'ShowFunction' value, return a list of 'Binding's.
--   If the domain of the given argument function is infinite,
--   the resulting list is infinite.
--
-- Some examples follow.  These are used as running examples in the definition
-- of 'explainedBindings', 'describedBindings' and 'clarifiedBindings'.
--
-- * Defined return values are represented as 'Just' 'String's:
--
--     > > bindings True
--     > [([],Just "True")]
--
-- * Undefined return values are represented as @Nothing@:
--
--     > > bindings undefined
--     > [([],Nothing)]
--
-- * Infinite domains result in an infinite bindings list:
--
--     > > bindings (id::Int->Int)
--     > [ (["0"], Just "0")
--     > , (["1"], Just "1")
--     > , (["-1"], Just "-1")
--     > , ...
--     > ]
--
-- * Finite domains result in a finite bindings list:
--
--     > > bindings (&&)
--     > [ (["False","False"], Just "False")
--     > , (["False","True"], Just "False")
--     > , (["True","False"], Just "False")
--     > , (["True","True"], Just "True")
--     > ]
--
--     > > bindings (||)
--     > [ (["False","False"], Just "False")
--     > , (["False","True"], Just "True")
--     > , (["True","False"], Just "True")
--     > , (["True","True"], Just "True")
--     > ]
--
-- * Even very simple functions are represented by an infinite list of bindings:
--
--     > > bindings (== 0)
--     > [ (["0"], Just "True")
--     > , (["1"], Just "False")
--     > , (["-1"], Just "False")
--     > , ...
--     > ]
--
--     > > bindings (== 1)
--     > [ (["0"], Just "False")
--     > , (["1"], Just "True")
--     > , (["-1"], Just "False")
--     > , ...
--     > ]
--
-- * Ignored arguments are still listed:
--
--     > > bindings ((\_ y -> y == 1) :: Int -> Int -> Bool)
--     > [ (["0","0"], Just "False")
--     > , (["0","1"], Just "True")
--     > , (["1","0"], Just "False")
--     > , ...
--     > ]
--
-- * Again, undefined values are represented as 'Nothing'.
--   Here, the 'head' of an empty list is undefined:
--
--     > > bindings (head :: [Int] -> Int)
--     > [ (["[]"], Nothing)
--     > , (["[0]"], Just "0")
--     > , (["[0,0]"], Just "0")
--     > , (["[1]"], Just "1")
--     > , ...
--     > ]
bindings :: ShowFunction a => a -> [Binding]
bindings  =  concat . bindtiers


-- | A drop-in implementation of 'bindtiers' for 'show'able types.
--
-- Define instances for 'show'able algebraic datatypes as:
--
-- > instance ShowFunction Ty where bindtiers  =  bindtiersShow
bindtiersShow :: Show a => a -> [[Binding]]
bindtiersShow x  =  [[([],errorToNothing $ show x)]]

instance ShowFunction ()       where  bindtiers  =  bindtiersShow
instance ShowFunction Bool     where  bindtiers  =  bindtiersShow
instance ShowFunction Int      where  bindtiers  =  bindtiersShow
instance ShowFunction Word     where  bindtiers  =  bindtiersShow
instance ShowFunction Integer  where  bindtiers  =  bindtiersShow
instance ShowFunction Char     where  bindtiers  =  bindtiersShow
instance ShowFunction Float    where  bindtiers  =  bindtiersShow
instance ShowFunction Double   where  bindtiers  =  bindtiersShow
instance ShowFunction Ordering where  bindtiers  =  bindtiersShow
instance Show a => ShowFunction [a]       where  bindtiers  =  bindtiersShow
instance Show a => ShowFunction (Maybe a) where  bindtiers  =  bindtiersShow

instance (Show a, Show b) => ShowFunction (Either a b) where
  bindtiers  =  bindtiersShow

instance (Show a, Show b) => ShowFunction (a,b) where
  bindtiers  =  bindtiersShow

-- instance for functional value type --
instance (Show a, Listable a, ShowFunction b) => ShowFunction (a->b) where
  bindtiers f  =  concatMapT bindtiersFor tiers
    where bindtiersFor x  =  mapFst (show x:) `mapT` bindtiers (f x)
          mapFst f (x,y)  =  (f x, y)

paren :: String -> String
paren s  =  "(" ++ s ++ ")"

showTuple :: [String] -> String
showTuple [x]  =  x
showTuple xs | all (== "_") xs  =  "_"
             | otherwise        =  paren $ intercalate "," xs

showBindings :: [Binding] -> [String]
showBindings bs  =  [ showTuple as ++ " -> " ++ r | (as, Just r) <- bs ]

showNBindings :: Bool -> Int -> [Binding] -> [String]
showNBindings infinite n bs'  =  take n bs
                              ++ ["..." | infinite || length bs > n]
  where
  bs  =  showBindings bs'

isValue :: ShowFunction a => a -> Bool
isValue f  =  case bindings f of
              [([],_)] -> True
              _        -> False

showValueOf :: ShowFunction a => a -> String
showValueOf x  =  case bindings x of
                  (_,Just x'):_ -> x'
                  _ -> "undefined"

-- | Given the number of patterns to show, shows a 'ShowFunction' value.
--
-- > > putStrLn $ showFunction undefined True
-- > True
-- >
-- > > putStrLn $ showFunction 3 (id::Int->Int)
-- > \x -> case x of
-- >       0 -> 0
-- >       1 -> 1
-- >       -1 -> -1
-- >       ...
-- >
-- > > putStrLn $ showFunction 4 (&&)
-- > \x y -> case (x,y) of
-- >         (True,True) -> True
-- >         _ -> False
-- >
--
-- In the examples above, "@...@" should be interpreted literally.
--
-- This can be used as an implementation of 'show' for functions:
--
-- > instance (Show a, Listable a, ShowFunction b) => Show (a->b) where
-- >   show  =  showFunction 8
--
-- See 'showFunctionLine' for an alternative without line breaks.
showFunction :: ShowFunction a => Int -> a -> String
showFunction n  =  showFunctionL False (n*n+1) n

-- | Same as 'showFunction', but has no line breaks.
--
-- > > putStrLn $ showFunctionLine 3 (id::Int->Int)
-- > \x -> case x of 0 -> 0; 1 -> 1; -1 -> -1; ...
-- > > putStrLn $ showFunctionLine 3 (&&)
-- > \x y -> case (x,y) of (True,True) -> True; _ -> False
--
-- This can be used as an implementation of 'show' for functions:
--
-- > instance (Show a, Listable a, ShowFunction b) => Show (a->b) where
-- >   show  =  showFunction 8
showFunctionLine :: ShowFunction a => Int -> a -> String
showFunctionLine n  =  showFunctionL True (n*n+1) n

-- | isUndefined checks if a function is totally undefined
--   for the given maximum number of values
isUndefined :: ShowFunction a => Int -> a -> Bool
isUndefined m  =  all (isNothing . snd) . take m . bindings

-- | checks if a function is constant
--   for the given maximum number of values
isConstant :: ShowFunction a => Int -> a -> Bool
isConstant m f  =  case take m $ bindings f of
                   []          -> False -- uninhabited type?
                   ((_,r'):bs) -> all (\(_,r) -> r == r') bs

-- | shows a constant function
showConstant :: ShowFunction a => Int -> a -> String
showConstant m f  =  "\\" ++ unwords vs ++ " -> " ++ fromMaybe "undefined" r
  where
  (as,r):_  =  bindings f
  vs  =  replicate (length as) "_"

-- The first boolean parameter tells if we are showing
-- the function on a single line
showFunctionL :: ShowFunction a => Bool -> Int -> Int -> a -> String
showFunctionL singleLine m n f
  | isValue f  =  showValueOf f
  | isConstant m f  =  showConstant m f
  | otherwise  =  lambdaPat ++ caseExp
  where
    lambdaPat  =  "\\" ++ unwords vs ++ " -> "
    casePat  =  "case " ++ showTuple (filter (/= "_") vs) ++ " of"
    (vs, bindings)  =  clarifiedBindings m n f
    bs  =  showNBindings (length bindings >= m) n bindings
    sep | singleLine  =  " "
        | otherwise  =  "\n"
    cases | singleLine  =  intercalate "; " bs
          | otherwise   =  unlines
                       $ (replicate (length lambdaPat) ' ' ++) `map` bs
    caseExp  =  if isUndefined m f
                then "undefined"
                else casePat ++ sep ++ cases

-- instances for further tuple arities --
instance (Show a, Show b, Show c)
      => ShowFunction (a,b,c) where  bindtiers  =  bindtiersShow
instance (Show a, Show b, Show c, Show d)
      => ShowFunction (a,b,c,d) where  bindtiers  =  bindtiersShow
instance (Show a, Show b, Show c, Show d, Show e)
      => ShowFunction (a,b,c,d,e) where  bindtiers  =  bindtiersShow
#ifndef __HUGS__
instance (Show a, Show b, Show c, Show d, Show e, Show f)
      => ShowFunction (a,b,c,d,e,f) where  bindtiers  =  bindtiersShow
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g)
      => ShowFunction (a,b,c,d,e,f,g) where  bindtiers  =  bindtiersShow
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
      => ShowFunction (a,b,c,d,e,f,g,h) where  bindtiers  =  bindtiersShow
instance ( Show a, Show b, Show c, Show d
         , Show e, Show f, Show g, Show h
         , Show i )
      => ShowFunction (a,b,c,d,e,f,g,h,i) where  bindtiers  =  bindtiersShow
instance ( Show a, Show b, Show c, Show d
         , Show e, Show f, Show g, Show h
         , Show i, Show j )
      => ShowFunction (a,b,c,d,e,f,g,h,i,j) where  bindtiers  =  bindtiersShow
instance ( Show a, Show b, Show c, Show d
         , Show e, Show f, Show g, Show h
         , Show i, Show j, Show k )
      => ShowFunction (a,b,c,d,e,f,g,h,i,j,k) where  bindtiers  =  bindtiersShow
instance ( Show a, Show b, Show c, Show d
         , Show e, Show f, Show g, Show h
         , Show i, Show j, Show k, Show l )
      => ShowFunction (a,b,c,d,e,f,g,h,i,j,k,l) where
  bindtiers  =  bindtiersShow
#endif

-- Data.Ratio
instance (Integral a, Show a) => ShowFunction (Ratio a) where
  bindtiers  =  bindtiersShow

-- Data.Complex
instance (RealFloat a, Show a) => ShowFunction (Complex a) where
  bindtiers  =  bindtiersShow

-- instance for types from Data.Int and Data.Word
instance ShowFunction Int8  where  bindtiers  =  bindtiersShow
instance ShowFunction Int16 where  bindtiers  =  bindtiersShow
instance ShowFunction Int32 where  bindtiers  =  bindtiersShow
instance ShowFunction Int64 where  bindtiers  =  bindtiersShow
instance ShowFunction Word8  where  bindtiers  =  bindtiersShow
instance ShowFunction Word16 where  bindtiers  =  bindtiersShow
instance ShowFunction Word32 where  bindtiers  =  bindtiersShow
instance ShowFunction Word64 where  bindtiers  =  bindtiersShow

-- instance for types from Test.LeanCheck.Utils.Types
instance ShowFunction Nat   where  bindtiers  =  bindtiersShow
instance ShowFunction Nat1  where  bindtiers  =  bindtiersShow
instance ShowFunction Nat2  where  bindtiers  =  bindtiersShow
instance ShowFunction Nat3  where  bindtiers  =  bindtiersShow
instance ShowFunction Nat4  where  bindtiers  =  bindtiersShow
instance ShowFunction Nat5  where  bindtiers  =  bindtiersShow
instance ShowFunction Nat6  where  bindtiers  =  bindtiersShow
instance ShowFunction Nat7  where  bindtiers  =  bindtiersShow
instance ShowFunction Int1  where  bindtiers  =  bindtiersShow
instance ShowFunction Int2  where  bindtiers  =  bindtiersShow
instance ShowFunction Int3  where  bindtiers  =  bindtiersShow
instance ShowFunction Int4  where  bindtiers  =  bindtiersShow
instance ShowFunction Word1 where  bindtiers  =  bindtiersShow
instance ShowFunction Word2 where  bindtiers  =  bindtiersShow
instance ShowFunction Word3 where  bindtiers  =  bindtiersShow
instance ShowFunction Word4 where  bindtiers  =  bindtiersShow

instance ShowFunction Natural where  bindtiers  =  bindtiersShow

instance ShowFunction Letter    where  bindtiers  =  bindtiersShow
instance ShowFunction AlphaNum  where  bindtiers  =  bindtiersShow
instance ShowFunction Digit     where  bindtiers  =  bindtiersShow
instance ShowFunction Alpha     where  bindtiers  =  bindtiersShow
instance ShowFunction Upper     where  bindtiers  =  bindtiersShow
instance ShowFunction Lower     where  bindtiers  =  bindtiersShow
instance ShowFunction Space     where  bindtiers  =  bindtiersShow

instance ShowFunction Spaces    where  bindtiers  =  bindtiersShow
instance ShowFunction Lowers    where  bindtiers  =  bindtiersShow
instance ShowFunction Uppers    where  bindtiers  =  bindtiersShow
instance ShowFunction Alphas    where  bindtiers  =  bindtiersShow
instance ShowFunction Digits    where  bindtiers  =  bindtiersShow
instance ShowFunction AlphaNums where  bindtiers  =  bindtiersShow
instance ShowFunction Letters   where  bindtiers  =  bindtiersShow

instance Show a => ShowFunction (X a) where  bindtiers  =  bindtiersShow
instance Show a => ShowFunction (Xs a) where  bindtiers  =  bindtiersShow
instance Show a => ShowFunction (Set a) where  bindtiers  =  bindtiersShow
instance Show a => ShowFunction (Bag a) where  bindtiers  =  bindtiersShow
instance Show a => ShowFunction (NoDup a) where  bindtiers  =  bindtiersShow
instance (Show a, Show b) => ShowFunction (Map a b) where
  bindtiers  =  bindtiersShow

-- misc instances
instance ShowFunction ExitCode   where  bindtiers  =  bindtiersShow
instance ShowFunction SeekMode   where  bindtiers  =  bindtiersShow
instance ShowFunction IOMode     where  bindtiers  =  bindtiersShow
instance ShowFunction BufferMode where  bindtiers  =  bindtiersShow
instance ShowFunction GeneralCategory where  bindtiers  =  bindtiersShow

-- instances for Foreign.C types
instance ShowFunction CChar      where  bindtiers  =  bindtiersShow
instance ShowFunction CSChar     where  bindtiers  =  bindtiersShow
instance ShowFunction CUChar     where  bindtiers  =  bindtiersShow
instance ShowFunction CShort     where  bindtiers  =  bindtiersShow
instance ShowFunction CUShort    where  bindtiers  =  bindtiersShow
instance ShowFunction CInt       where  bindtiers  =  bindtiersShow
instance ShowFunction CUInt      where  bindtiers  =  bindtiersShow
instance ShowFunction CLong      where  bindtiers  =  bindtiersShow
instance ShowFunction CULong     where  bindtiers  =  bindtiersShow
instance ShowFunction CPtrdiff   where  bindtiers  =  bindtiersShow
instance ShowFunction CSize      where  bindtiers  =  bindtiersShow
instance ShowFunction CWchar     where  bindtiers  =  bindtiersShow
instance ShowFunction CSigAtomic where  bindtiers  =  bindtiersShow
instance ShowFunction CLLong     where  bindtiers  =  bindtiersShow
instance ShowFunction CULLong    where  bindtiers  =  bindtiersShow
instance ShowFunction CIntPtr    where  bindtiers  =  bindtiersShow
instance ShowFunction CUIntPtr   where  bindtiers  =  bindtiersShow
instance ShowFunction CIntMax    where  bindtiers  =  bindtiersShow
instance ShowFunction CUIntMax   where  bindtiers  =  bindtiersShow
instance ShowFunction CClock     where  bindtiers  =  bindtiersShow
instance ShowFunction CTime      where  bindtiers  =  bindtiersShow
instance ShowFunction CFloat     where  bindtiers  =  bindtiersShow
instance ShowFunction CDouble    where  bindtiers  =  bindtiersShow
#if __GLASGOW_HASKELL__ >= 802
instance ShowFunction CBool      where  bindtiers  =  bindtiersShow
#endif
#if __GLASGOW_HASKELL__
instance ShowFunction CUSeconds  where  bindtiers  =  bindtiersShow
instance ShowFunction CSUSeconds where  bindtiers  =  bindtiersShow
#endif

-- | Returns a set of variables and a set of bindings
--   describing how a function works.
--
-- Some argument values are generalized to "@_@" when possible.
-- If one of the function arguments is not used altogether, it is ommited in
-- the set of bindings and appears as "_" in the variables list.
--
-- This is the /last/ function in the clarification pipeline.
--
-- It takes two integer arguments:
--
-- 1. @m@: the maximum number of cases considered for computing the description;
-- 2. @n@: the maximum number of cases in the actual description.
--
-- As a general rule of thumb, set @m=n*n+1@.
--
-- Some examples follow:
--
-- * When all arguments are used, the result is the same as 'describedBindings':
--
--     > > clarifiedBindings 100 10 (==1)
--     > ( ["x"], [ (["1"],Just "True"),
--     >          , (["_"],Just "False") ] )
--
-- * When some arguments are unused, they are omitted in the list of bindings
--   and appear as @"_"@ in the list of variables.
--
--     > > clarifiedBindings 100 10 (\_ y -> y == 1)
--     > ( ["_", "y"], [ (["1"],Just "True")
--     >               , (["_"],Just "False") ] )
clarifiedBindings :: ShowFunction a => Int -> Int -> a -> ([String],[Binding])
clarifiedBindings m n  =  clarifyBindings . describedBindings m n

clarifyBindings :: [Binding] -> ([String],[Binding])
clarifyBindings bs  =  (varnamesByUsage used, map (mapFst $ select used) bs)
  where
  mapFst f (x,y)  =  (f x, y)
  used  =  usedArgs bs

varnamesByUsage :: [Bool] -> [String]
varnamesByUsage  =  zipWith used varnames
  where
  used s False  =  "_"
  used s True   =  s
  varnames  =  ["x","y","z","w"] ++ map (++"'") varnames

usedArgs :: [Binding] -> [Bool]
usedArgs  =  foldr1 (zipWith (||))
          .  map (map (/= "_") . fst)

-- | Returns a set of bindings describing how a function works.
-- Some argument values are generalized to "@_@" when possible.
-- It takes two integer arguments:
--
-- 1. @m@: the maximum number of cases considered for computing description;
-- 2. @n@: the maximum number of cases in the actual description.
--
-- As a general rule of thumb, set @m=n*n+1@.
--
-- This is the /second/ function in the clarification pipeline.
--
-- This function processes the result of 'explainedBindings'
-- to sometimes return shorter descriptions.
-- It chooses the shortest of the following (in order):
--
-- * regular unexplained-undescribed 'bindings';
-- * regular 'explainedBindings';
-- * 'explainedBindings' with least occurring cases generalized first;
--
-- Here are some examples:
--
-- * Sometimes the result is the same as 'explainedBindings':
--
--     > > describedBindings 100 10 (||)
--     > [ (["False","False"],Just "False")
--     > , (["_","_"],Just "True") ]
--
--     > > describedBindings 100 10 (==0)
--     > [ (["0"],Just "True")
--     > , (["_"],Just "False") ]
--
-- * but sometimes it is shorter because we consider generalizing least
--   occurring cases first:
--
--     > > describedBindings 100 10 (&&)
--     > [ ( ["True","True"],Just "True")
--     > , ( ["_","_"],Just "False") ]
--
--     > > describedBindings 100 10 (==1)
--     > [ (["1"],Just "True"),
--     > , (["_"],Just "False") ]
--
--     > > describedBindings 100 10 (\_ y -> y == 1)
--     > [ (["_","1"],Just "True")
--     > , (["_","_"],Just "False") ]
describedBindings :: ShowFunction a => Int -> Int -> a -> [Binding]
describedBindings m n f
  | length bs1 <= n  =  bs1
  | otherwise        =  bs0
  where
  bs0  =  take m $ bindings f
  bs1  =  describeBindings bs0

describeBindings :: [Binding] -> [Binding]
describeBindings bs  =  minimumOn length
  [ bs
  , explainBindings bs
  , explainBindings . concat . sortOn length $ classifyOn snd bs
  ]

-- | Returns a set of bindings explaining how a function works.
--   Some argument values are generalized to "@_@" when possible.
--   It takes as argument the maximum number of cases
--   considered for computing the explanation.
--
-- A measure of success in this generalization process is if this function
-- returns less values than the asked maximum number of cases.
--
-- This is the /first/ function in the clarification pipeline.
--
-- * In some cases, 'bindings' cannot be "explained"
--   an almost unchanged result of 'bindings' is returned
--   with the last binding having variables replaced by "@_@":
--
--     > > explainedBindings 4 (id::Int->Int)
--     > [ (["0"],Just "0")
--     > , (["1"],Just "1")
--     > , (["-1"],Just "-1")
--     > , (["_"],Just "2") ]
--
-- * When possible, some cases are generalized using @_@:
--
--     > > explainedBindings 10 (||)
--     > [ (["False","False"],Just "False")
--     > , (["_","_"],Just "True") ]
--
--     but the resulting "explanation" might not be the shortest possible
--     (cf. 'describedBindings'):
--
--     > > explainedBindings 10 (&&)
--     > [ ( ["False","_"],Just "False")
--     > , (["_","False"],Just "False")
--     > , (["_","_"],Just "True") ]
--
-- * Generalization works for infinite domains (heuristically):
--
--     > > explainedBindings 10 (==0)
--     > [ (["0"],Just "True")
--     > , (["_"],Just "False") ]
--
-- * Generalization for each item is processed in the order they are generated
--   by 'bindings' hence explanations are not always the shortest possible
--   (cf. 'describedBindings').  In the following examples, the first case is
--   redundant.
--
--     > > explainedBindings 10 (==1)
--     > [ (["0"],Just "False")
--     > , (["1"],Just "True"),
--     > , (["_"],Just "False") ]
--
--     > > explainedBindings 10 (\_ y -> y == 1)
--     > [ (["_","0"],Just "False")
--     > , (["_","1"],Just "True")
--     > , (["_","_"],Just "False") ]
explainedBindings :: ShowFunction a => Int -> a -> [Binding]
explainedBindings m  =  explainBindings . take m . bindings

explainBindings :: [Binding] -> [Binding]
explainBindings  =  explain []
  where
  explain :: [Binding] -> [Binding] -> [Binding]
  explain bs' []           =  reverse bs'
  explain bs' ((as,r):bs)  =  explain (bs''++bs') [b | b <- bs
                                                     , none (b <~~) bs'']
    where
    bs''  =  discardLater (<~~)
          [ (gas,r) | gas <- generalizations as
                    , and [r' == r | (as',r') <- bs, as' <~ gas] ]

generalizations :: [String] -> [[String]]
generalizations []      =  [[]]
generalizations (v:vs)  =  map ("_":) gvs ++ map (v:) gvs
  where
  gvs  =  generalizations vs

-- | Should be read as "is generalized by":
--
-- > > ["1","2","3"] <~ ["_","_","_"]
-- > True
-- > > ["_","_","_"] <~ ["1","2","3"]
-- > False
-- > > ["1","3"] <~ ["_","3"]
-- > True
-- > > ["_","3"] <~ ["_","4"]
-- > False
(<~) :: [String] -> [String] -> Bool
[]     <~ []        =  True
(v:vs) <~ ("_":ws)  =  vs <~ ws
(v:vs) <~ (w:ws)    =  v == w && vs <~ ws
_      <~ _         =  False

-- | Should be read as "is generalized by".
(<~~) :: Binding -> Binding -> Bool
(as,r) <~~ (as',r')  =  as <~ as' && r == r'


-- general auxiliary functions

discard :: (a -> Bool) -> [a] -> [a]
discard p  =  filter (not . p)

discardLater :: (a -> a -> Bool) -> [a] -> [a]
discardLater (?>)  =  dl
  where
  dl []      =  []
  dl (x:xs)  =  x : discard (?> x) (dl xs)

none :: (a -> Bool) -> [a] -> Bool
none p  =  not . any p

-- sortOn is only available on GHC > 7.8
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f  =  sortBy (compare `on` f)

-- likewise...
minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f  =  minimumBy (compare `on` f)

select :: [Bool] -> [a] -> [a]
select [] _  =  []
select _ []  =  []
select (p:ps) (x:xs)  =  if p then x : xs' else xs'  where  xs' = select ps xs
