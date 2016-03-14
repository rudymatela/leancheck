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
-- TODO: (ShowFunction) allow showing of undefined values
-- TODO: (ShowFunction) document exported functions

import Test.Check.Core
import Data.List

type Binding = ([String], String)

class ShowFunction a where
  tBindings :: a -> [[Binding]]

bindings :: ShowFunction a => a -> [Binding]
bindings = concat . tBindings


-- instances for (algebraic/numeric) data types --
tBindingsShow :: Show a => a -> [[Binding]]
tBindingsShow x = [[ ([],show x) ]]

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

showBindingsOf :: ShowFunction a => a -> [String]
showBindingsOf = map showBinding . bindings
  where showBinding (as,r) = showTuple as ++ " -> " ++ r

showNBindingsOf :: ShowFunction a => Int -> a -> [String]
showNBindingsOf n f =
  if length bs > n
    then take n bs ++ ["..."]
    else bs
  where bs = take (n+1) $ showBindingsOf f

showValueOf :: ShowFunction a => a -> String
showValueOf = snd . head . bindings

showFunction :: ShowFunction a => Int -> a -> String
showFunction = showFunctionL False

showFunctionLine :: ShowFunction a => Int -> a -> String
showFunctionLine = showFunctionL True

-- The first boolean parameter tells if we are showing
-- the function on a single line
showFunctionL :: ShowFunction a => Bool -> Int -> a -> String
showFunctionL singleLine n f =
  if null vs
    then showValueOf f
    else lambdaPat ++ casePat ++ (if singleLine then " " ++ bsS else "\n" ++ bsM)
  where vs = varnamesFor f
        bs = showNBindingsOf n f
        lambdaPat = "\\" ++ unwords vs ++ " -> "
        casePat = "case " ++ showTuple vs ++ " of"
        bsS = intercalate "; " bs
        bsM = unlines (map (replicate (length lambdaPat + 2) ' ' ++) bs)


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
