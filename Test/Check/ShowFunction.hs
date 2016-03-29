-- | This module exports the 'ShowFunction' typeclass,
--   its instances and related functions.
--
-- Using this module, it is possible to implement
-- a Show instance for functions:
--
-- > import Test.Check.ShowFunction
-- > instance (Show a, Listable a, ShowFunction b) => Show (a->b) where
-- >   show = showFunction 8
--
-- This shows functions as a case pattern with up to 8 cases.
--
-- The module
-- @Test.Check.Function.Show@ ('Test.Check.Function.Show')
-- exports an instance like the one above.
module Test.Check.ShowFunction
  ( showFunction
  , showFunctionLine
  , Binding
  , bindings
  , ShowFunction (..)
  , tBindingsShow
  -- * Re-exports
  , Listable
  )
where

import Test.Check.Core
import Test.Check.Error (errorToNothing)
import Data.List
import Data.Maybe

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

instance ShowFunction ()   where tBindings = tBindingsShow
instance ShowFunction Bool where tBindings = tBindingsShow
instance ShowFunction Int  where tBindings = tBindingsShow
instance ShowFunction Char where tBindings = tBindingsShow
instance Show a => ShowFunction [a]       where tBindings = tBindingsShow
instance Show a => ShowFunction (Maybe a) where tBindings = tBindingsShow
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
showTuple [x] = x
showTuple xs  = paren $ intercalate "," xs

showNBindingsOf :: ShowFunction a => Int -> Int -> a -> [String]
showNBindingsOf m n f = take n bs
                     ++ ["..." | length bs' >= m || length bs > n]
  where bs' = take m $ bindings f
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

-- | isUndefined checks if a function is totally undefined.
-- When it is not possible to check all values, it returns false
isUndefined :: ShowFunction a => Int -> a -> Bool
isUndefined m f = length bs < m && all (isNothing . snd) bs
  where bs = take m $ bindings f

-- The first boolean parameter tells if we are showing
-- the function on a single line
showFunctionL :: ShowFunction a => Bool -> Int -> Int -> a -> String
showFunctionL singleLine m n f | isValue f = showValueOf f
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
                       $ (replicate (length lambdaPat + 2) ' ' ++) `map` bs
    caseExp = if isUndefined m f
                then "undefined"
                else casePat ++ sep ++ cases

-- instances for further tuples --
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
