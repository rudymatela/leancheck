-- | This module exports the 'ShowFunction' typeclass,
--   its instances and related functions.
module Test.Check.ShowFunction
  ( ShowFunction
  , Listable
  , showFunction
  , showFunctionLine
  , bindings
  )
where
-- TODO: (ShowFunction) document exported functions

import Test.Check.Core
import Test.Check.Error (errorToNothing)
import Data.List
import Data.Maybe

type Binding = ([String], Maybe String)

class ShowFunction a where
  tBindings :: a -> [[Binding]]

bindings :: ShowFunction a => a -> [Binding]
bindings = concat . tBindings


-- instances for (algebraic/numeric) data types --
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
  tBindings f = tConcatMap tBindingsFor tiers
    where tBindingsFor x = mapFst (show x:) `tmap` tBindings (f x)
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

showFunction :: ShowFunction a => Int -> a -> String
showFunction n = showFunctionL False (n*n+1) n

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
