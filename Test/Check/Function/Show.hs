-- | An experimental Show instance for functions.
--
-- It only works for functions with one argument.
module Test.Check.Function.Show () where

import Test.Check
import Data.Function (on)
import Data.List (intercalate)

instance (Show a, Show b, Eq b, Listable a) => Show (a -> b) where
  show = showBindings . take 10 . functionToPairs

showBindings :: (Show a, Show b, Eq b) => [(a,b)] -> String
showBindings ps =
  case limit3 ((==) `on` snd) ps of
    Nothing  -> "[" ++ showPairs ps ++ "]"
    Just ps' -> "[" ++ showPairs ps' ++ "...]"

showPairs :: (Show a, Show b) => [(a,b)] -> String
showPairs = intercalate ", " . map showPair

showPair :: (Show a, Show b) => (a,b) -> String
showPair (x,y) = show x ++ " " ++ show y

limit3 :: (a -> a -> Bool) -> [a] -> Maybe [a]
limit3 eq [] = Nothing
limit3 eq [x] = Nothing
limit3 eq [x,y] = Nothing
limit3 eq [x,y,z] = Nothing
limit3 eq (x:y:z:xs) = if x `eq` y && y `eq` z
                         then Just [x{-,y,z-}]
                         else (x:) `fmap` limit3 eq (y:z:xs)

functionToPairs :: (Listable a) => (a -> b) -> [(a,b)]
functionToPairs f = map (\a -> (a,f a)) list

-- Little *rough* idea to implement an instance that works for
-- functions of N arguments.
-- (while not depend on special GHC extensions)
--
-- We would need a class (similar to ShowMutable of FitSpec):
--
-- class ShowFunction where
--   functionShow :: Int -> FunctionS
--
-- Data FunctionS = Atom String | Function String Function | etc...
--
-- The default instance for data types would be something like:
--
-- functionShowData = Atom . show
--
-- For functions:
--
-- instance (Show a, ShowFunction b) => ShowFunction (a -> b) where
--   ...
--
--
-- The actual show instance would be:
--
-- instance (Show a, ShowFunction b) => Show (a -> b) where
--   show = showFunctionS . flatten . showFunction 100
--
-- Test.Check.Function.ShowFunction -- Export ShowFunction instance
-- Test.Check.Function.Show -- Export show instance
--
-- To think:
--
-- Infinite structure?  With infinite tiers?
-- Then a flatten infinite structure with simple lists?
-- Then a function to "crop" it?
--
-- Maybe? a default instance for container data types (tuples / lists):
--
-- functionShowDataContainer = ...
